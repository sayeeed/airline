package com.patson

import com.patson.PassengerSimulation.PassengerConsumptionResult
import com.patson.model._
import com.patson.data._

import scala.collection.mutable._
import scala.collection.{immutable, mutable}
import com.patson.model.airplane.{Airplane, AirplaneMaintenanceUtil, LinkAssignments}
import com.patson.model.event.Olympics

import scala.util.Random
import com.patson.model.oil.OilPrice
import com.patson.util.AllianceCache

import java.util.concurrent.ThreadLocalRandom

object LinkSimulation {
  val FUEL_UNIT_COST = OilPrice.DEFAULT_UNIT_COST * 94 //for easier flight monitoring, let's make it the default unit price here
  val FUEL_DISTANCE_EXPONENT = 1.4
  val FUEL_EMPTY_AIRCRAFT_BURN_PERCENT = 0.62
  val CREW_UNIT_COST = 6.75
  val CREW_BASE_COST = 50
  val CREW_EQ_EXPONENT = 1.95


  def linkSimulation(cycle: Int) : (List[LinkConsumptionDetails], scala.collection.immutable.Map[Lounge, LoungeConsumptionDetails], immutable.Map[(PassengerGroup, Airport, Route), Int], immutable.Map[Int, AirlinePaxStat]) = {
    println("Loading all links")
    val links = LinkSource.loadAllLinks(LinkSource.FULL_LOAD)
    val flightLinks = links.filter(_.transportType == TransportType.FLIGHT).map(_.asInstanceOf[Link])
    println("Finished loading all links")

    //val demand = Await.result(DemandGenerator.computeDemand(), Duration.Inf)'
    val demand = DemandGenerator.computeDemand(cycle)
    println("DONE with demand total demand: " + demand.foldLeft(0) {
      case(holder, (_, _, demandValue)) =>  
        holder + demandValue
    })

    simulateLinkError(flightLinks)
    
    val PassengerConsumptionResult(consumptionResult: scala.collection.immutable.Map[(PassengerGroup, Airport, Route), Int], missedPassengerResult : immutable.Map[(PassengerGroup, Airport), Int])= PassengerSimulation.passengerConsume(demand, links)

    println("Generating Airline Stats")
    val airlineStats = tallyPassengerTypesByAirline(consumptionResult)

    //used for airport stats
    println("Generating flight stats")
    val linkStatistics = generateFlightStatistics(consumptionResult, cycle)
    println("Saving generated stats to DB")
    LinkStatisticsSource.deleteLinkStatisticsBeforeCycle(cycle - 2) //was cycle - 5
    LinkStatisticsSource.saveLinkStatistics(linkStatistics)

    //generate country market share
    println("Generating country market share")
    val countryMarketShares = generateCountryMarketShares(consumptionResult)
    println("Saving country market share to DB")
    CountrySource.saveMarketShares(countryMarketShares)

    //generate Olympics stats
    EventSource.loadEvents().filter(_.isActive(cycle)).foreach { event =>
      event match {
        case olympics : Olympics =>
          println("Generating Olympics stats")
          val olympicsConsumptions = consumptionResult.filter {
            case ((passengerGroup, _, _), _) => passengerGroup.passengerType == PassengerType.OLYMPICS
          }
          val missedOlympicsPassengers = missedPassengerResult.filter {
            case ((passengerGroup, _), _) => passengerGroup.passengerType == PassengerType.OLYMPICS
          }
          val olympicsCountryStats = generateOlympicsCountryStats(cycle, olympicsConsumptions, missedOlympicsPassengers)
          EventSource.saveOlympicsCountryStats(olympics.id, olympicsCountryStats)
          val olympicsAirlineStats = generateOlympicsAirlineStats(cycle, olympicsConsumptions)
          EventSource.saveOlympicsAirlineStats(olympics.id, olympicsAirlineStats)
          println("Generated olympics country stats")
        case _ => //
      }

    }

    //save all consumptions
    var startTime = System.currentTimeMillis()
    println("Saving " + consumptionResult.size +  " consumptions")
    ConsumptionHistorySource.updateConsumptions(consumptionResult)
    var endTime = System.currentTimeMillis()
    println(s"Saved all consumptions. Took ${endTime - startTime} millisecs")

    println("Calculating profits by links")
    startTime = System.currentTimeMillis()
    val linkConsumptionDetails = ListBuffer[LinkConsumptionDetails]()
    val loungeConsumptionDetails = ListBuffer[LoungeConsumptionDetails]()
    val allAirplaneAssignments: immutable.Map[Int, LinkAssignments] = AirplaneSource.loadAirplaneLinkAssignmentsByCriteria(List.empty)
    //cost by link
    val costByLink = mutable.HashMap[Transport, ListBuffer[PassengerCost]]()
    consumptionResult.foreach {
      case((passengerGroup, airport, route), passengerCount) => route.links.foreach { linkConsideration =>
        costByLink.getOrElseUpdate(linkConsideration.link, ListBuffer[PassengerCost]()).append(PassengerCost(passengerGroup, passengerCount, linkConsideration.cost))
      }
    }

    links.foreach {
      case flightLink : Link =>
        if (flightLink.capacity.total > 0) {
          val (linkResult, loungeResult) = computeLinkAndLoungeConsumptionDetail(flightLink, cycle, allAirplaneAssignments, costByLink.getOrElse(flightLink, List.empty).toList)
          linkConsumptionDetails += linkResult
          loungeConsumptionDetails ++= loungeResult
        }
      case nonFlightLink => //only compute for flights (class Link)
        linkConsumptionDetails += LinkConsumptionDetails(nonFlightLink, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, cycle)
    }

    endTime = System.currentTimeMillis()
    println(s"Finished calculation on profits by links. Took ${endTime - startTime} millisecs")

    purgeAlerts()
    checkLoadFactor(flightLinks, cycle)

    LinkSource.deleteLinkConsumptionsByCycle(300)
    LinkSource.saveLinkConsumptions(linkConsumptionDetails.toList)

    println("Calculating Lounge usage")
    //condense the lounge result
    val loungeResult : scala.collection.immutable.Map[Lounge, LoungeConsumptionDetails] = loungeConsumptionDetails.groupBy(_.lounge).map{
      case (lounge, consumptionsForThisLounge) =>
        var totalSelfVisitors = 0
        var totalAllianceVistors = 0
        consumptionsForThisLounge.foreach {
          case LoungeConsumptionDetails(_, selfVisitors, allianceVisitors, _) =>
            totalSelfVisitors += selfVisitors
            totalAllianceVistors += allianceVisitors
        }
        (lounge, LoungeConsumptionDetails(lounge, totalSelfVisitors, totalAllianceVistors, cycle))
    }.toMap

    LoungeHistorySource.updateConsumptions(loungeResult.map(_._2).toList)
    //purge older result
    LoungeHistorySource.deleteConsumptionsBeforeCycle(cycle)


    (linkConsumptionDetails.toList, loungeResult, consumptionResult, airlineStats)
  }

  case class PassengerCost(group : PassengerGroup, passengerCount : Int, cost : Double)

  val minorDelayNormalThreshold = 0.3
  val majorDelayNormalThreshold = 0.1
  val cancellationNormalThreshold = 0.03
  val minorDelayCriticalThreshold = 0.5
  val majorDelayCriticalThreshold = 0.2
  val cancellationCriticalThreshold = 0.05

  def simulateLinkError(links : List[Link]) = {
    links.foreach {
      link => {
        var i = 0
        val assignedInServiceAirplanes = link.getAssignedAirplanes().filter(_._1.isReady)
        for ( i <- 0 until link.frequency) {
          val airplaneCount : Int = assignedInServiceAirplanes.size
          if (airplaneCount > 0) {
            val airplane = assignedInServiceAirplanes.toList.map(_._1)(i % airplaneCount) //round-robin
            val errorValue = ThreadLocalRandom.current().nextDouble()
            val hangarCountFrom = link.from.getAirlineBase(link.airline.id).map(_.specializations.count(_.getType == BaseSpecializationType.HANGAR)).getOrElse(0)
            val hangarCountTo = link.to.getAirlineBase(link.airline.id).map(_.specializations.count(_.getType == BaseSpecializationType.HANGAR)).getOrElse(0)
            val airplaneCondition = Math.min(100, (hangarCountTo + hangarCountFrom) * 4 + airplane.condition)
            val conditionMultiplier = (Airplane.MAX_CONDITION - airplaneCondition * 0.75).toDouble / Airplane.MAX_CONDITION

            if (airplane.condition > Airplane.CRITICAL_CONDITION) { //small chance of delay and cancellation
              if (errorValue < cancellationNormalThreshold * conditionMultiplier) {
                link.cancellationCount = link.cancellationCount + 1
              } else if (errorValue < majorDelayNormalThreshold * conditionMultiplier) {
                link.majorDelayCount = link.majorDelayCount + 1
              } else if (errorValue < minorDelayNormalThreshold * conditionMultiplier) {
                link.minorDelayCount = link.minorDelayCount + 1
              }
            } else {
              if (errorValue < cancellationCriticalThreshold * conditionMultiplier) {
                link.cancellationCount = link.cancellationCount + 1
              } else if (errorValue < majorDelayCriticalThreshold * conditionMultiplier) {
                link.majorDelayCount = link.majorDelayCount + 1
              } else if (errorValue < minorDelayCriticalThreshold * conditionMultiplier) {
                link.minorDelayCount = link.minorDelayCount + 1
              }
            }
          }
        }
      }
      if (link.cancellationCount > 0) {
        link.addCancelledSeats(link.capacityPerFlight() * link.cancellationCount)
      }
    }
  }

  /**
    * Only called by test cases
    * @param link
    * @param cycle
    * @return
    */
  def computeFlightLinkConsumptionDetail(link : Link, cycle : Int) : LinkConsumptionDetails = {
    //for testing, assuming all airplanes are only assigned to this link
    val assignmentsToThis = link.getAssignedAirplanes().filter(_._1.isReady).toList.map {
      case(airplane, assignment) => (airplane.id, LinkAssignments(immutable.Map(link.id -> assignment)))
    }.toMap
    computeLinkAndLoungeConsumptionDetail(link, cycle, assignmentsToThis, List.empty)._1
  }

  def computeLinkAndLoungeConsumptionDetail(link : Link, cycle : Int, allAirplaneAssignments : immutable.Map[Int, LinkAssignments], passengerCostEntries : List[PassengerCost]) : (LinkConsumptionDetails, List[LoungeConsumptionDetails]) = {
    val flightLink = link.asInstanceOf[Link]


    val fuelCost = flightLink.getAssignedModel() match {
      case Some(model) =>
        val loadFactor = FUEL_EMPTY_AIRCRAFT_BURN_PERCENT + (1 - FUEL_EMPTY_AIRCRAFT_BURN_PERCENT) * flightLink.getTotalSoldSeats.toDouble / flightLink.capacity.totalwithSeatSize
        val distanceFactor = 1 + 0.1 * Math.pow(flightLink.duration.toDouble / 60, FUEL_DISTANCE_EXPONENT * loadFactor)
        val fuelCost = FUEL_UNIT_COST * model.capacity * distanceFactor * (model.ascentBurn * loadFactor + model.cruiseBurn * link.distance / 800)

        (fuelCost * (flightLink.frequency - flightLink.cancellationCount)).toInt
      case None => 0
    }

    val fuelTaxRate = AirlineGrades.findTaxRate(link.airline.getReputation())
    val fuelTax = (fuelCost * (fuelTaxRate.toDouble / 100)).toInt

    val inServiceAssignedAirplanes = flightLink.getAssignedAirplanes().filter(_._1.isReady)
    //the % of time spent on this link for each airplane
    val assignmentWeights : immutable.Map[Airplane, Double] = { //0 to 1
      inServiceAssignedAirplanes.view.map {
        case(airplane, assignment) =>
          allAirplaneAssignments.get(airplane.id) match {
            case Some(linkAssignmentsToThisAirplane) =>
              val weight : Double = assignment.flightMinutes.toDouble / linkAssignmentsToThisAirplane.assignments.values.map(_.flightMinutes).sum
              (airplane, weight)
            case None => (airplane, 1.0) //100%
          } //it shouldn't be else...but just to play safe, if it's not found in "all" table, assume this is the only link assigned
      }.toMap
    }
    var maintenanceCost = 0
    inServiceAssignedAirplanes.foreach {
      case(airplane, _) =>
        maintenanceCost += (airplane.model.baseMaintenanceCost * assignmentWeights(airplane)).toInt
    }
    maintenanceCost = (maintenanceCost * AirplaneMaintenanceUtil.getMaintenanceFactor(link.airline.id)).toInt


    val airportFees = flightLink.getAssignedModel() match {
      case Some(model) =>
        val airline = flightLink.airline
        (flightLink.from.slotFee(model, airline) + flightLink.to.slotFee(model, airline)) * flightLink.frequency + flightLink.from.landingFee(flightLink.getTotalSoldSeats) + flightLink.to.landingFee(flightLink.getTotalSoldSeats)
      case None => 0
    }

    var depreciation = 0
    inServiceAssignedAirplanes.foreach {
      case(airplane, _) =>
        depreciation += (airplane.depreciationRate * assignmentWeights(airplane)).toInt
    }

    val targetQualityCost = Math.pow(flightLink.airline.getTargetServiceQuality().toDouble / 22, CREW_EQ_EXPONENT)
    var crewCost = CREW_BASE_COST
    var inflightCost, revenue = 0
    val crewUnitCost = if (link.airline.airlineType == AirlineType.DISCOUNT || link.airline.airlineType == AirlineType.BEGINNER) CREW_UNIT_COST * 0.75 else CREW_UNIT_COST
    LinkClass.values.foreach { linkClass =>
      val capacity = flightLink.capacity(linkClass)
      val soldSeats = flightLink.soldSeats(linkClass)

      inflightCost += computeInflightCost(linkClass.resourceMultiplier, flightLink, soldSeats)
      crewCost += (targetQualityCost * capacity * linkClass.resourceMultiplier * flightLink.duration / 60).toInt + (crewUnitCost * capacity * linkClass.resourceMultiplier * flightLink.duration / 60).toInt
      revenue += soldSeats * flightLink.price(linkClass)
    }

    // delays incur extra cost
    var delayCompensation = Computation.computeCompensation(flightLink)

    // lounge cost
    val fromLounge = flightLink.from.getLounge(flightLink.airline.id, flightLink.airline.getAllianceId(), activeOnly = true)
    val toLounge = flightLink.to.getLounge(flightLink.airline.id, flightLink.airline.getAllianceId(), activeOnly = true)
    var loungeCost = 0
    val loungeConsumptionDetails = ListBuffer[LoungeConsumptionDetails]()
    if (fromLounge.isDefined || toLounge.isDefined) {
      val visitorCount = flightLink.soldSeats(BUSINESS) + flightLink.soldSeats(FIRST)
      if (fromLounge.isDefined) {
        loungeCost += visitorCount * Lounge.PER_VISITOR_CHARGE
        loungeConsumptionDetails += (
          if (fromLounge.get.airline.id == flightLink.airline.id) {
            LoungeConsumptionDetails(fromLounge.get, selfVisitors = visitorCount, allianceVisitors = 0, cycle)
          } else {
            LoungeConsumptionDetails(fromLounge.get, selfVisitors = 0, allianceVisitors = visitorCount, cycle)
          })
      }
      if (toLounge.isDefined) {
        loungeCost += visitorCount * Lounge.PER_VISITOR_CHARGE
        loungeConsumptionDetails += (
          if (toLounge.get.airline.id == flightLink.airline.id) {
            LoungeConsumptionDetails(toLounge.get, selfVisitors = visitorCount, allianceVisitors = 0, cycle)
          } else {
            LoungeConsumptionDetails(toLounge.get, selfVisitors = 0, allianceVisitors = visitorCount, cycle)
          })
      }

    }

    val profit = revenue - fuelCost - fuelTax - maintenanceCost - crewCost - airportFees - inflightCost - delayCompensation - depreciation - loungeCost

    //calculation overall satisifaction
    var satisfactionTotalValue : Double = 0
    var totalPassengerCount = 0
    passengerCostEntries.foreach {
      case PassengerCost(passengerGroup, passengerCount, cost) =>
        val preferredLinkClass = passengerGroup.preference.preferredLinkClass
        val standardPrice = flightLink.standardPrice(preferredLinkClass, passengerGroup.passengerType)
        val loadFactor = link.soldSeats.totalwithSeatSize / link.capacity.totalwithSeatSize
        val satisfaction = Computation.computePassengerSatisfaction(cost, standardPrice, loadFactor)
        satisfactionTotalValue += satisfaction * passengerCount
        totalPassengerCount += passengerCount
    }
    val overallSatisfaction = if (totalPassengerCount == 0) 0 else satisfactionTotalValue / totalPassengerCount

    val result = LinkConsumptionDetails(flightLink, fuelCost, fuelTax, crewCost, airportFees, inflightCost, delayCompensation = delayCompensation, maintenanceCost, depreciation = depreciation, loungeCost = loungeCost, revenue, profit, overallSatisfaction, cycle)
    //println("model : " + link.getAssignedModel().get + " profit : " + result.profit + " result: " + result)
    (result, loungeConsumptionDetails.toList)
  }

  //"service supplies"
  val computeInflightCost = (classMultiplier : Double, link : Link, soldSeats : Int) => {
    val durationCostPerHour: Double =
      if (link.rawQuality <= 20) {
        -5 //selling food & credit cards :)
      } else if (link.rawQuality <= 40) {
        -1
      } else if (link.rawQuality <= 60) {
        4
      } else if (link.rawQuality <= 80) {
        9
      } else {
        15
      }
    val airlineTypeMultipler = link.airline.airlineType match {
      case AirlineType.BEGINNER => 0.7
      case AirlineType.NOSTALGIA => 0.7
      case _ => 1.0
    }

    val costPerPassenger = classMultiplier * durationCostPerHour * airlineTypeMultipler * link.duration.toDouble / 60
    (costPerPassenger * soldSeats).toInt
  }

  val LOAD_FACTOR_ALERT_LINK_COUNT_THRESHOLD = 3 //how many airlines before load factor is checked
  val LOAD_FACTOR_ALERT_THRESHOLD = 0.5 //LF threshold
  val LOAD_FACTOR_ALERT_DURATION = 52

  /**
    * Purge alerts that are no longer valid
    */
  def purgeAlerts() = {
    //only purge link cancellation alerts for now
    val existingAlerts = AlertSource.loadAlertsByCategory(AlertCategory.LINK_CANCELLATION)

    //try to purge the alerts, as some alerts might get inserted while the link is deleted during the simulation time
    val liveLinkIds : List[Int] = LinkSource.loadAllLinks(LinkSource.ID_LOAD).map(_.id)
    val deadAlerts = existingAlerts.filter(alert => alert.targetId.isDefined && !liveLinkIds.contains(alert.targetId.get))
    AlertSource.deleteAlerts(deadAlerts)
    println("Purged alerts with no corresponding links... " + deadAlerts.size)
  }

  def checkLoadFactor(links : List[Link], cycle : Int) = {
    val existingAlerts = AlertSource.loadAlertsByCategory(AlertCategory.LINK_CANCELLATION)

    //group links by from and to airport ID Tuple(id1, id2), smaller ID goes first in the tuple
    val linksByAirportIds = links.filter(_.capacity.total > 0).filter(_.airline.airlineType != AirlineType.NON_PLAYER).groupBy( link =>
      if (link.from.id < link.to.id) (link.from.id, link.to.id) else (link.to.id, link.from.id)
    )

    val existingAlertsByLinkId : scala.collection.immutable.Map[Int, Alert] = existingAlerts.map(alert => (alert.targetId.get, alert)).toMap

    val updatingAlerts = ListBuffer[Alert]()
    val newAlerts = ListBuffer[Alert]()
    val deletingAlerts = ListBuffer[Alert]()
    val deletingLinks = ListBuffer[Link]()
    val newLogs = ListBuffer[Log]()

    linksByAirportIds.foreach {
      case((airportId1, airportId2), links) =>
        if (links.size >= LOAD_FACTOR_ALERT_LINK_COUNT_THRESHOLD) {
          links.foreach { link =>
            val loadFactor = link.getTotalSoldSeats.toDouble / link.getTotalCapacity
            if (loadFactor < LOAD_FACTOR_ALERT_THRESHOLD) {
              existingAlertsByLinkId.get(link.id) match {
                case Some(existingAlert) => //continue to have problem
                  if (existingAlert.duration <= 1) { //kaboom! deleting
                    deletingAlerts.append(existingAlert)
                    deletingLinks.append(link)
                    val message = "Airport authorities have revoked license of " + link.airline.name + " to operate route between " +  link.from.displayText + " and " + link.to.displayText + " due to prolonged low load factor"
                    newLogs += Log(airline = link.airline, message = message, category = LogCategory.LINK, severity = LogSeverity.WARN, cycle = cycle)
                    //notify competitors too with lower severity
                    links.filter(_.id != link.id).foreach { competitorLink =>
                      newLogs += Log(airline = competitorLink.airline, message = message, category = LogCategory.LINK, severity = LogSeverity.INFO, cycle = cycle)
                    }
                  } else { //clock is ticking!
                     updatingAlerts.append(existingAlert.copy(duration = existingAlert.duration -1))
                  }
                case None => //new warning
                  val message = "Airport authorities have issued warning to " + link.airline.name + " on low load factor of route between " +  link.from.displayText + " and " + link.to.displayText + ". If the load factor remains lower than " + LOAD_FACTOR_ALERT_THRESHOLD * 100 + "% for the remaining duration, the license to operate this route will be revoked!"
                  val alert = Alert(airline = link.airline, message = message, category = AlertCategory.LINK_CANCELLATION, targetId = Some(link.id), cycle = cycle, duration = LOAD_FACTOR_ALERT_DURATION)
                  newAlerts.append(alert)
              }
            } else { //LF good, delete existing alert if any
              existingAlertsByLinkId.get(link.id).foreach { existingAlert =>
                deletingAlerts.append(existingAlert)
              }
            }
          }
        } else { //not enough competitor, check if alert should be removed
          links.foreach { link =>
            existingAlertsByLinkId.get(link.id).foreach { existingAlert =>
              deletingAlerts.append(existingAlert)
            }
          }
        }
    }


    deletingLinks.foreach { link =>
       println("Revoked link: " + link)
       LinkSource.deleteLink(link.id)
    }
    AlertSource.updateAlerts(updatingAlerts.toList)
    AlertSource.insertAlerts(newAlerts.toList)
    AlertSource.deleteAlerts(deletingAlerts.toList)

    LogSource.insertLogs(newLogs.toList)
  }

  def generateFlightStatistics(consumptionResult: scala.collection.immutable.Map[(PassengerGroup, Airport, Route), Int], cycle : Int) : List[LinkStatistics] = {
    val statistics = Map[LinkStatisticsKey, Int]()
    consumptionResult.foreach {
      case ((_, _, route), passengerCount) =>
        for (i <- 0 until route.links.size) {
          val link = route.links(i)
          if (link.link.transportType == TransportType.FLIGHT) { //only do stats on flights here
            val airline = link.link.airline
            val key =
              if (i == 0) {
                if (route.links.size == 1) {
                  LinkStatisticsKey(link.from, link.to, true, true, airline)
                } else {
                  LinkStatisticsKey(link.from, link.to, true, false, airline)
                }
              } else if (i == route.links.size - 1) { //last one in list
                LinkStatisticsKey(link.from, link.to, false, true, airline)
              } else { //in the middle
                LinkStatisticsKey(link.from, link.to, false, false, airline)
              }
            val newPassengerCount = statistics.getOrElse(key, 0) + passengerCount
            statistics.put(key, newPassengerCount)
          }
        }
    }
    
    statistics.map { 
      case (linkStatisticsKey, passenger) =>
        LinkStatistics(linkStatisticsKey, passenger, cycle)
    }.toList
    
  }

  def tallyPassengerTypesByAirline(consumptionResult: scala.collection.immutable.Map[(PassengerGroup, Airport, Route), Int]): immutable.Map[Int, AirlinePaxStat] = {
    // Create a mutable map to accumulate stats per airline
    val airlineStatsMap = mutable.Map[Int, (Int, Int, Int, Int, Int)]() // (tourist, elite, business, total, allianceRoute)

    // First, group by route to avoid double-counting Codeshares
    val routesByPassengerType = consumptionResult.groupBy(_._1._3)

    routesByPassengerType.foreach {
      case (route, passengerTypes) =>
        // Check if this route has alliance partners for each airline
        def hasAlliancePartners(airlineId: Int) = route.links.exists(linkConsideration =>
          linkConsideration.link.transportType == TransportType.FLIGHT &&
            linkConsideration.link.airline.id == airlineId &&
            linkConsideration.link.airline.getAllianceId().isDefined &&
            AllianceCache.isEstablishedAndValid(linkConsideration.link.airline.getAllianceId().get, airlineId) &&
          route.links.exists(otherLink => 
            otherLink.link.transportType == TransportType.FLIGHT &&
            otherLink.link.airline.id != airlineId &&
            otherLink.link.airline.getAllianceId().isDefined &&
            otherLink.link.airline.getAllianceId() == linkConsideration.link.airline.getAllianceId() &&
            AllianceCache.isEstablishedAndValid(linkConsideration.link.airline.getAllianceId().get, airlineId)
          )
        )

        // Process each airline in the route
        route.links.filter(_.link.transportType == TransportType.FLIGHT).foreach { link =>
          val airlineId = link.link.airline.id
          val currentStats = airlineStatsMap.getOrElse(airlineId, (0, 0, 0, 0, 0))
          val codeshares = if (hasAlliancePartners(airlineId)) passengerTypes.values.sum else 0
          
          // Process each passenger type for this airline
          val newStats = passengerTypes.foldLeft(currentStats) {
            case ((tourist, elite, business, total, alliance), ((passengerGroup, _, _), passengerCount)) =>
              passengerGroup.passengerType match {
                case PassengerType.TOURIST => (tourist + passengerCount, elite, business, total + passengerCount, alliance)
                case PassengerType.ELITE => (tourist, elite + passengerCount, business, total + passengerCount, alliance)
                case PassengerType.BUSINESS => (tourist, elite, business + passengerCount, total + passengerCount, alliance)
                case _ => (tourist, elite, business, total + passengerCount, alliance)
              }
          }

          // Add Codeshares after processing all passenger types
          airlineStatsMap.put(airlineId, (newStats._1, newStats._2, newStats._3, newStats._4, newStats._5 + codeshares))
        }
    }

    airlineStatsMap.map {
    case (airlineId, (tourist, elite, business, total, allianceRoute)) =>
      airlineId -> AirlinePaxStat(tourist, elite, business, total, allianceRoute)
    }.toMap
  }
  
  def generateCountryMarketShares(consumptionResult: scala.collection.immutable.Map[(PassengerGroup, Airport, Route), Int]) : List[CountryMarketShare] = {
    val countryAirlinePassengers = Map[String, Map[Int, Long]]()
    consumptionResult.foreach {
      case ((_, _, route), passengerCount) =>
        for (i <- 0 until route.links.size) {
          val link = route.links(i)
          if (link.link.transportType == TransportType.FLIGHT) {
            val airline = link.link.airline
            val country = link.from.countryCode
            val airlinePassengers = countryAirlinePassengers.getOrElseUpdate(country, Map[Int, Long]())
            val currentSum : Long = airlinePassengers.getOrElse(airline.id, 0L)
            airlinePassengers.put(airline.id, currentSum + passengerCount)
          }
        }
    }

    countryAirlinePassengers.map {
      case ((countryCode, airlinePassengers)) => { 
        CountryMarketShare(countryCode, airlinePassengers.toMap)
      }
    }.toList

  }

  case class PassengerTransportStats(cycle : Int, transported : Int, total : Int)
  /**
    * Stats on how much pax from a country was carried/missed
    * @param olympicsConsumptions
    * @param missedOlympicsPassengers
    * @return Map[countryCode, transportRate]
    */
  def generateOlympicsCountryStats(cycle : Int, olympicsConsumptions: immutable.Map[(PassengerGroup, Airport, Route), Int], missedOlympicsPassengers: immutable.Map[(PassengerGroup, Airport), Int]) : immutable.Map[String, PassengerTransportStats] = {
    val passengersByCountry = mutable.HashMap[String, Int]()
    val missedPassengersByCountry = mutable.HashMap[String, Int]()

    val allCountries = mutable.HashSet[String]()
    olympicsConsumptions.foreach {
      case ((passengerGroup, _, _), passengerCount) =>
        val countryCode = passengerGroup.fromAirport.countryCode
        val currentCount = passengersByCountry.getOrElse(countryCode, 0)
        passengersByCountry.put(countryCode, currentCount + passengerCount)
        allCountries.add(countryCode)
    }

    missedOlympicsPassengers.foreach {
      case ((passengerGroup, _), passengerCount) =>
        val countryCode = passengerGroup.fromAirport.countryCode
        val currentCount = missedPassengersByCountry.getOrElse(countryCode, 0)
        missedPassengersByCountry.put(countryCode, currentCount + passengerCount)
        allCountries.add(countryCode)
    }


    allCountries.map { countryCode =>
      val transportStats =
        passengersByCountry.get(countryCode) match {
          case Some(passengers) => missedPassengersByCountry.get(countryCode) match {
            case Some(missedPassengers) => PassengerTransportStats(cycle, passengers, (passengers + missedPassengers))
            case None => PassengerTransportStats(cycle, passengers, passengers)
          }
          case None => PassengerTransportStats(cycle, 0, missedPassengersByCountry.getOrElse(countryCode, 0))
        }
      (countryCode, transportStats)
    }.toMap
  }


  /**
    *
    * @param olympicsConsumptions
    * @return Map[airline, scope] score if 1 if Airline A has direct flight that takes the pax to olympics city, otherwise each airline in the route get 1 / n, which n is the number of hops
    */
  def generateOlympicsAirlineStats(cycle : Int, olympicsConsumptions: immutable.Map[(PassengerGroup, Airport, Route), Int]) : immutable.Map[Airline, (Int, BigDecimal)] = {
    val scoresByAirline = mutable.HashMap[Airline, BigDecimal]()

    olympicsConsumptions.foreach {
      case ((_, _, Route(links, _, _, _)), passengerCount) =>
        links.foreach { link =>
          if (link.link.transportType == TransportType.FLIGHT) {
            val existingScore : BigDecimal = scoresByAirline.getOrElse(link.link.airline, 0)
            scoresByAirline.put(link.link.airline, existingScore + passengerCount.toDouble / links.size)
          }
        }
    }

    scoresByAirline.view.mapValues( score => (cycle, score)).toMap
  }

  /**
    * Refresh link capacity and frequency if necessary
    */
  def refreshLinksPostCycle() = {
    println("Refreshing link capacity and frequency to find discrepancies")
    val simpleLinks = LinkSource.loadAllLinks(LinkSource.ID_LOAD)
    val fullLinks = LinkSource.loadAllLinks(LinkSource.FULL_LOAD).map(link => (link.id, link)).toMap
    println("Finished loading both the simple and full links")
    //not too ideal, but even if someone update the link assignment when this is in progress, it should be okay, as that assignment
    //is suppose to update the link capacity and frequency anyway
    simpleLinks.foreach { simpleLink =>
      fullLinks.get(simpleLink.id).foreach { fullLink =>
        if (simpleLink.frequency != fullLink.frequency || simpleLink.capacity != fullLink.capacity) {
          println(s"Adjusting capacity/frequency of  $simpleLink to $fullLink")
          LinkSource.updateLink(fullLink)
        }
      }
    }
  }

  def simulatePostCycle(cycle : Int) = {
    //now update the link capacity if necessary
    LinkSimulation.refreshLinksPostCycle()
    purgeNegotiationCoolDowns(cycle)
  }

  def purgeNegotiationCoolDowns(cycle: Int): Unit = {
    LinkSource.purgeNegotiationCoolDowns(cycle)
    NegotiationSource.deleteLinkDiscountBeforeExpiry(cycle)
  }
}
