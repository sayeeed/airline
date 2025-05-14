package com.patson.ai

import com.patson.model.Airline
import com.patson.model.Airport
import com.patson.data.LinkSource
import com.patson.DemandGenerator
import com.patson.model.Computation
import scala.util.Random
import com.patson.model.LinkClassValues
import com.patson.model.airplane.LinkAssignment
import com.patson.model.airplane.Airplane
import com.patson.data.CountrySource
import com.patson.model.Link
import com.patson.model.AirlineBase
import com.patson.data.AirlineSource
import com.patson.model.AirlineCashFlowItem
import com.patson.model.CashFlowType
import com.patson.data.CycleSource
import com.patson.model.DelegateTask
import com.patson.model.BusyDelegate
import com.patson.data.DelegateSource

object AISimUtil {
  private lazy val countryRelationships = CountrySource.getCountryMutualRelationships()

  /* 
  
    Route Management

   */

  def negotiateLink(airline: Airline, link: Link) : Option[Link] = {
    // start negotiation process
    val linkCost = Computation.getLinkCreationCost(link.from, link.to)
    val existingLink : Option[Link] = LinkSource.loadFlightLinkByAirportsAndAirline(link.from.id, link.to.id, airline.id)
    val negotiationInfo = NegotiationUtil.getLinkNegotiationInfo(airline, link, existingLink)
    val delegateCount = Math.ceil(negotiationInfo.finalRequirementValue).toInt
    // if the airline has enough delegates to negotiate, negotiate link
    if (delegateCount <= airline.getDelegateInfo().availableCount && delegateCount <= NegotiationUtil.MAX_ASSIGNED_DELEGATE) {
      val negotiationResultOption =
        if(negotiationInfo.finalRequirementValue > 0) {
          Some(NegotiationUtil.negotiate(negotiationInfo, delegateCount))
        } else {
          None
        }

      // if negotiation is successful, return the link and deduct cash for link creation
      if (negotiationResultOption.map(_.isSuccessful).getOrElse(true)) {
        println(s"${airline.name} added a new link (${link.from.iata}-${link.to.iata}) with ${link.capacity} total capacity.")

        AirlineSource.saveCashFlowItem(AirlineCashFlowItem(airline.id, CashFlowType.CREATE_LINK, linkCost * -1))
        AirlineSource.adjustAirlineBalance(airline.id, linkCost * -1)

        Some(link)
      }

      // regardless of the negotiation is successful or a failure, we'll need to update delegates
      negotiationResultOption.foreach { negotiationResult =>
        //update delegate status
        val cycle = CycleSource.loadCycle()
        val task = DelegateTask.linkNegotiation(cycle, link.from, link.to)
        val coolDown = if (negotiationResult.isSuccessful) task.coolDown else task.coolDown / 2 //half cooldown if it was unsuccessful
        val availableCycle = cycle + coolDown
  
        val busyDelegates = (0 until delegateCount).toList.map { _ =>
          BusyDelegate(airline, task, Some(availableCycle))
        }
  
        DelegateSource.saveBusyDelegates(busyDelegates)
        LinkSource.saveNegotiationCoolDown(airline, link.from, link.to, cycle + Link.LINK_NEGOTIATION_COOL_DOWN)
      }
      None
    }
    None
  }

  def isLinkSaturated(airline: Airline, fromAirport: Airport, toAirport: Airport) : Boolean = {
    val linksByAirport = LinkSource.loadFlightLinksByAirports(fromAirport.id, toAirport.id)
    val rivalLinksByAirport = linksByAirport.filterNot(_.airline.id == airline.id)
    val airlineLinksByAirport = linksByAirport.filter(_.airline.id == airline.id)
    val demand = getLinkDemand(fromAirport, toAirport)

    var totalCapacity = 0
    var totalSoldSeats = 0

    // if rival links is not empty, add up total capacity and sold seats
    if (rivalLinksByAirport.nonEmpty) rivalLinksByAirport.foreach { link => 
      totalCapacity += link.getTotalCapacity
      totalSoldSeats += link.getTotalSoldSeats
    }
    // if current airline's links are not empty, add up total capacity and sold seats
    if (airlineLinksByAirport.nonEmpty) airlineLinksByAirport.foreach { link => 
      totalCapacity += link.getTotalCapacity  
      totalSoldSeats += link.getTotalSoldSeats
    }

    if (totalCapacity < DemandGenerator.addUpDemands(demand)) return false
    else if (totalCapacity > 0 && totalSoldSeats / totalCapacity > 0.85) return true
    else return false
  }

  /* 
   * Returns the Demand of a link given the from Airport and to Airport
   */
  def getLinkDemand(fromAirport: Airport, toAirport: Airport) : DemandGenerator.Demand = {
    val distance = Computation.calculateDistance(fromAirport, toAirport)
    val relationship = countryRelationships.getOrElse((fromAirport.countryCode, toAirport.countryCode), 0)
    val affinity = Computation.calculateAffinityValue(fromAirport.zone, toAirport.zone, relationship)
    val demand = DemandGenerator.computeBaseDemandBetweenAirports(fromAirport, toAirport, affinity, distance)
    demand
  }

  def calculateTotalCapacity(assignedAirplanes: Map[Airplane, LinkAssignment]): LinkClassValues = {
    assignedAirplanes.map { case (airplane, assignment) =>
      LinkClassValues(airplane.configuration.economyVal, airplane.configuration.businessVal, airplane.configuration.firstVal) * assignment.frequency
    }.reduce(_ + _)
  }

  def drawFromPool(poolTopFirst: Seq[Airport], drawSize: Int): Seq[Airport] = {
    if (drawSize >= poolTopFirst.length) {
      poolTopFirst
    } else {
      Random.shuffle(poolTopFirst).take(drawSize)
    }
  }

  def computeAverageLoadFactor(link: Link, cycle: Int) : (Int, Int, Int, Int) = {
    var cycleCount = 10
    // if current cycle is less than cycleCount, only check back the amount of current cycles
    if(cycle < cycleCount) { cycleCount = cycle }
    
    var totalEconomySold = 0
    var totalEconomyCapacity = 0
    var totalBusinessSold = 0
    var totalBusinessCapacity = 0
    var totalFirstSold = 0
    var totalFirstCapacity = 0

    val linkConsumptions = LinkSource.loadLinkConsumptionsByLinkId(link.id, cycleCount)
    linkConsumptions.foreach { linkConsumption =>
      totalEconomySold += linkConsumption.link.soldSeats.economyVal
      totalEconomyCapacity += linkConsumption.link.capacity.economyVal

      totalBusinessSold += linkConsumption.link.soldSeats.businessVal
      totalBusinessCapacity += linkConsumption.link.capacity.businessVal

      totalFirstSold += linkConsumption.link.soldSeats.firstVal
      totalFirstCapacity += linkConsumption.link.capacity.firstVal        
    }

    val economyLoadFactor = if (totalEconomyCapacity > 0) ((totalEconomySold.toDouble / totalEconomyCapacity) * 100).toInt else 0
    val businessLoadFactor = if (totalBusinessCapacity > 0) ((totalBusinessSold.toDouble / totalBusinessCapacity) * 100).toInt else 0
    val firstLoadFactor = if (totalFirstCapacity > 0) ((totalFirstSold.toDouble / totalFirstCapacity) * 100).toInt else 0

    val totalSold = totalEconomySold + totalBusinessSold + totalFirstSold
    val totalCapacity = totalEconomyCapacity + totalBusinessCapacity + totalFirstCapacity
    val totalLoadFactor = if (totalCapacity > 0) ((totalSold.toDouble / totalCapacity) * 100).toInt else 0

    (economyLoadFactor, businessLoadFactor, firstLoadFactor, totalLoadFactor)
  }

  def getAverageRivalPrice(link: Link, rivalLinks: List[Link]) : (Int, Int, Int) = {
    var economyPrice = 0
    var businessPrice = 0
    var firstPrice = 0
    var avgEconomyPrice = 0
    var avgBusinessPrice = 0
    var avgFirstPrice = 0

    if (rivalLinks.size > 0) {
      rivalLinks.foreach { rivalLink =>
        economyPrice += rivalLink.price.economyVal
        businessPrice += rivalLink.price.businessVal
        firstPrice += rivalLink.price.firstVal
      }

      avgEconomyPrice = economyPrice / rivalLinks.size
      avgBusinessPrice = businessPrice / rivalLinks.size
      avgFirstPrice = firstPrice / rivalLinks.size
    }

    (avgEconomyPrice, avgBusinessPrice, avgFirstPrice)
  }

  def getTotalRivalCapacity(link: Link, rivalLinks: List[Link]) : (Int, Int, Int) = {
    var totalEconomyCapacity = 0
    var totalBusinessCapacity = 0
    var totalFirstCapacity = 0

    if (rivalLinks.size > 0) {
      rivalLinks.foreach { rivalLink =>
        totalEconomyCapacity += rivalLink.capacity.economyVal
        totalBusinessCapacity += rivalLink.capacity.businessVal
        totalFirstCapacity += rivalLink.capacity.firstVal
      }
    }

    (totalEconomyCapacity, totalBusinessCapacity, totalFirstCapacity)
  }

  def isCompetition(link: Link, rivalLinks: List[Link]) : Boolean = {
    if (rivalLinks.isEmpty) { return false } else return true
  }

  /* 
  
    BASE MANAGEMENT
  
   */

  def canUpgradeBase(airline: Airline, base: AirlineBase, cashFlow: Long) : Boolean = {
    val multiplier = base.scale match {
      case 1 | 2 => 1
      case 3 | 4 => 2
      case 5 | 6 => 3
      case 7 | 8 => 5
      case 9 | 10 => 8
      case 11 | 12 => 12
      case _ => 14
    }

    val safeSpendingLimit = cashFlow * 6 * multiplier
    val upgradeCost = base.calculateUpgradeCost(base.scale + 1)

    if ((airline.getBalance() - upgradeCost > 1000000) && safeSpendingLimit > upgradeCost) return true
    return false
  }

  def getLowestScaleBase(bases: List[AirlineBase]) : AirlineBase = {
    var lowestScaleBase = getHeadquarter(bases)
    bases.foreach { base =>
      if (base.scale < lowestScaleBase.scale) lowestScaleBase = base
    }

    lowestScaleBase
  }

  def getLowestCapacityBase(bases: List[AirlineBase], airline: Airline, linksByFromAirport: Map[Int, List[Link]]) : AirlineBase = {
    var lowestCapacityBase = getHeadquarter(bases)
    var capacityDifference = 0
    bases.foreach { base =>
      if (base.getOfficeStaffCapacity - getCurrentBaseStaffRequired(airline, base, linksByFromAirport) > capacityDifference) {
        capacityDifference = base.getOfficeStaffCapacity - getCurrentBaseStaffRequired(airline, base, linksByFromAirport)
        lowestCapacityBase = base
      }
    }

    lowestCapacityBase
  }

  def isBaseMaxCapacity(airline: Airline, base: AirlineBase, linksByFromAirport: Map[Int, List[Link]]) : Boolean = {
    val linksFromBase = linksByFromAirport.get(base.airport.id)
    val currentBaseStaff = getCurrentBaseStaffRequired(airline, base, linksByFromAirport)
    if (linksFromBase.nonEmpty && currentBaseStaff > (base.getOfficeStaffCapacity - 15)) {
      return true
    }

    return false
  }

  def allBasesAtCapacity(bases: List[AirlineBase], airline: Airline, linksByFromAirport: Map[Int, List[Link]]) : Boolean = {
    bases.foreach { base =>
      if (!isBaseMaxCapacity(airline, base, linksByFromAirport)) {
        return false
      }
    }

    return true
  }
  
  def canBaseSupportLink(airline: Airline, link: Link, flightLinksByAirline: Map[Int, List[Link]], homeAirport: Airport) : Boolean = {
    val base = airline.getBases().find(_.airport == homeAirport).get
    val linksByFromAirport = flightLinksByAirline.get(airline.id).getOrElse(List.empty).groupBy(_.from.id)
    val currentBaseStaff = getCurrentBaseStaffRequired(airline, base, linksByFromAirport)

    if (base.airport == homeAirport && (link.getCurrentOfficeStaffRequired + currentBaseStaff) <= base.getOfficeStaffCapacity) {
      return true
    }

    return false
  }

  def getCurrentBaseStaffRequired(airline: Airline, base: AirlineBase, linksByFromAirport: Map[Int, List[Link]]) : Int = {
    val currentBaseStaff = linksByFromAirport.get(base.airport.id) match {
      case Some(links) => links.map(_.getCurrentOfficeStaffRequired).sum
      case None => 0
    }

    currentBaseStaff
  }

  def getHeadquarter(bases: List[AirlineBase]) : AirlineBase = {
    bases.foreach { base =>
      if (base.headquarter) return base
    }

    return bases.head
  }
}
