package com.patson

import com.patson.data._
import com.patson.model._
import com.patson.model.airplane.Model
import com.patson.model.airplane.AirplaneConfiguration.first
import scala.util.Random
import scala.collection.mutable
import com.patson.DemandGenerator.Demand
import com.patson.model.airplane.Airplane
import com.patson.model.airplane.LinkAssignment
import com.patson.model.airplane.AirplaneConfiguration

/* AI Design

- Route Management
- Pricing
- Finances
- Fleet Management

 */

object AISimulation {
  def simulateAIAirlines(cycle: Int) = {
    if (cycle % 1 == 0) {
      val aiAirlines = AirlineSource.loadAllAirlines(true).filterNot(_.aiType == AIType.PLAYER)
      val allFlightLinks = LinkSource.loadAllFlightLinks()
      val flightLinksByAirline = allFlightLinks.groupBy(_.airline.id)

      aiAirlines.foreach {
        case airline : Airline =>
          if (Random.nextDouble() < 1) {
            val airlineFlightLinks = flightLinksByAirline.getOrElse(airline.id, Nil)
            routeManagement(airline, airlineFlightLinks, cycle)
          }
      }
    } 
  }

  // route management
  // - update pricing on unprofitable/low load factor routes
  // - increase pricing on full load factor routes
  private def routeManagement(airline: Airline, flightLinks: List[Link], cycle: Int) = {
    flightLinks.foreach {
      case flightLink : Link =>
        //println("Updating pricing for Airline: " + flightLink.airline.name + " Link: " + flightLink.from.iata + "-" + flightLink.to.iata)
        val linkConsumptions = LinkSource.loadLinkConsumptionsByLinkId(flightLink.id, cycle).headOption
        val rivalFlightLinks = LinkSource.loadFlightLinksByAirports(flightLink.from.id, flightLink.to.id).filterNot(_.airline.id == flightLink.airline.id)

        linkConsumptions.foreach { linkConsumption =>
          val basePrice = LinkClassValues(linkConsumption.link.price.economyVal, linkConsumption.link.price.businessVal, linkConsumption.link.price.firstVal)
          // all 10-week averages
          val (economyLF, businessLF, firstLF, totalLF) = computeAverageLoadFactor(flightLink, cycle)
          val (avgEconomyPrice, avgBusinessPrice, avgFirstPrice) = getAverageRivalPrice(flightLink, rivalFlightLinks)
            
          // adjusts prices down if total load factor is below 90% and no major delays or cancellations occurred
          adjustPrices(flightLink, linkConsumption, basePrice)
          adjustFrequency(flightLink, linkConsumption, rivalFlightLinks, totalLF, basePrice)
        }
    }
  }

  /* 
   * Adjusts the price (economy, business, first all separated) of the specified flight link by 5%:
   *  - Down if the respective load factor is below a certain percentage (e.g. 90) and the current 
   *    percentage off base price is no less than 75% (to prevent prices spiraling down)
   *  - Up if the respective load factor is equal to 100%
   */
  private def adjustPrices(flightLink: Link, linkConsumption: LinkConsumptionDetails, basePrice: LinkClassValues) = {
    val basePrice = LinkClassValues(linkConsumption.link.price.economyVal, linkConsumption.link.price.businessVal, linkConsumption.link.price.firstVal)
    var newEconomyPrice = flightLink.price.economyVal.toDouble
    var newBusinessPrice = flightLink.price.businessVal.toDouble
    var newFirstPrice = flightLink.price.firstVal.toDouble
              
    if (linkConsumption.link.capacity.economyVal > 0) {
      val economyLoadFactor = ((linkConsumption.link.soldSeats.economyVal.toDouble / linkConsumption.link.capacity.economyVal.toDouble) * 100).toInt
      val economyPercentage = (flightLink.price.economyVal.toDouble / basePrice.economyVal.toDouble)
      if (economyLoadFactor < 90 && economyPercentage > 0.75) { newEconomyPrice = (basePrice.economyVal * (economyPercentage - 0.05)).toInt }
      if (economyLoadFactor == 100) { newEconomyPrice = (basePrice.economyVal * (economyPercentage + 0.05)).toInt }
    }

    if (linkConsumption.link.capacity.businessVal > 0) {
      val businessLoadFactor = ((linkConsumption.link.soldSeats.businessVal.toDouble / linkConsumption.link.capacity.businessVal.toDouble) * 100).toInt
      val businessPercentage = (flightLink.price.businessVal.toDouble / basePrice.businessVal.toDouble)
      if (businessLoadFactor < 90 && businessPercentage > 0.75) { newBusinessPrice = (basePrice.businessVal * (businessPercentage - 0.05)).toInt }
      if (businessLoadFactor == 100) { newBusinessPrice = (basePrice.businessVal * (businessPercentage + 0.05)).toInt }
    }
              
    if (linkConsumption.link.capacity.firstVal > 0) {
      val firstLoadFactor = ((linkConsumption.link.soldSeats.firstVal.toDouble / linkConsumption.link.capacity.firstVal.toDouble) * 100).toInt
      val firstPercentage = (flightLink.price.firstVal.toDouble / basePrice.firstVal.toDouble)
      if (firstLoadFactor < 90 && firstPercentage > 0.75) { newFirstPrice = (basePrice.firstVal * (firstPercentage - 0.05)).toInt }
      if (firstLoadFactor == 100) { newFirstPrice = (basePrice.firstVal * (firstPercentage + 0.05)).toInt }
    }

    if (newEconomyPrice != flightLink.price.economyVal || newBusinessPrice != flightLink.price.businessVal || newFirstPrice != flightLink.price.firstVal) {
      val newPrices = LinkClassValues(newEconomyPrice.toInt, newBusinessPrice.toInt, newFirstPrice.toInt)
      val newLink = flightLink.copy(price = newPrices)
      println("Updated the above link to new prices: " + newLink)
      LinkSource.updateLink(newLink)
    }
  }

  private def adjustFrequency(flightLink: Link, linkConsumption: LinkConsumptionDetails, rivalFlightLinks: List[Link], totalLF: Int, basePrice: LinkClassValues) = {
    lazy val countryRelationships = CountrySource.getCountryMutualRelationships()
    val relationship = countryRelationships.getOrElse((flightLink.from.countryCode, flightLink.to.countryCode), 0)
    val affinity = Computation.calculateAffinityValue(flightLink.from.zone, flightLink.to.zone, relationship)
    val demand = DemandGenerator.computeBaseDemandBetweenAirports(flightLink.from, flightLink.to, affinity, flightLink.distance)
    var linkTotalCapacity = flightLink.getTotalCapacity
    var linkTotalSoldSeats = flightLink.getTotalSoldSeats

    if (rivalFlightLinks.size > 0) {
      rivalFlightLinks.foreach { rivalFlightLink =>
        linkTotalCapacity += rivalFlightLink.getTotalCapacity
        linkTotalSoldSeats += rivalFlightLink.getTotalSoldSeats
      }
    }

    if (totalLF >= 98 && (linkTotalSoldSeats.toDouble / linkTotalCapacity >= 0.90 || DemandGenerator.addUpDemands(demand) > linkTotalCapacity)) {
      // try increase freq

    } else if (totalLF < 90 && flightLink.price.economyVal <= (basePrice.economyVal * 0.75)) {
      // decrease freq
      if (flightLink.frequency <= 3) {
        // try and downsize plane? or delete route
        LinkSource.deleteLink(flightLink.id)
      } else {
        // reduce frequency to next multiple of 3
        val newFrequency = Math.max(3, (flightLink.frequency / 3) * 3)
        val maxFrequencyPerAirplane = Computation.calculateMaxFrequency(flightLink.getAssignedModel().get, flightLink.distance)
        val airplanesRequired = Math.max(1, newFrequency / maxFrequencyPerAirplane)
        val assignedAirplanes = updateAssignedPlanes(flightLink.getAssignedModel().get, flightLink.airline, flightLink.from, newFrequency, flightLink.distance, airplanesRequired, maxFrequencyPerAirplane)
        val newLink = flightLink.copy(frequency = newFrequency)

        newLink.setAssignedAirplanes(assignedAirplanes)

        println("Updating link with new frequency")
        LinkSource.updateLink(newLink)
        LinkSource.updateAssignedPlanes(newLink.id, assignedAirplanes)
      }
    }
  }

  private def computeAverageLoadFactor(link: Link, cycle: Int) : (Int, Int, Int, Int) = {
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

    val economyLoadFactor = if (totalEconomyCapacity > 0) totalEconomySold.toDouble / totalEconomyCapacity else 0.0
    val businessLoadFactor = if (totalBusinessCapacity > 0) totalBusinessSold.toDouble / totalBusinessCapacity else 0.0
    val firstLoadFactor = if (totalFirstCapacity > 0) totalFirstSold.toDouble / totalFirstCapacity else 0.0

    val totalSold = totalEconomySold + totalBusinessSold + totalFirstSold
    val totalCapacity = totalEconomyCapacity + totalBusinessCapacity + totalFirstCapacity
    val totalLoadFactor = if (totalCapacity > 0) totalSold.toDouble / totalCapacity else 0.0

    (economyLoadFactor.toInt, businessLoadFactor.toInt, firstLoadFactor.toInt, totalLoadFactor.toInt)
  }

  private def getAverageRivalPrice(link: Link, rivalLinks: List[Link]) : (Int, Int, Int) = {
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

  private def getTotalRivalCapacity(link: Link, rivalLinks: List[Link]) : (Int, Int, Int) = {
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

  private def isCompetition(link: Link, rivalLinks: List[Link]) : Boolean = {
    if (rivalLinks.isEmpty) { return false } else return true
  }

  // finances


  // fleet management

  private def updateAssignedPlanes(model: Model, airline: Airline, homeAirport: Airport, frequency: Int, distance: Int, airplanesRequired: Int, maxFrequencyPerAirplane: Int) : Map[Airplane, LinkAssignment] = {
    val assignedAirplanes = mutable.Map[Airplane, LinkAssignment]()
    val flightMinutesRequired = Computation.calculateFlightMinutesRequired(model, distance)
    var remainingFrequency = frequency
    val allAirplanes = AirplaneSource.loadAirplanesByOwner(airline.id)
    val availablePlanesOfModel = allAirplanes.filter(_.model == model).toBuffer
    // Map to track each plane's remaining minutes
    val airplaneRemainingMinutes = availablePlanesOfModel.map { airplane =>
      airplane -> (airplane.availableFlightMinutes)
    }.filter(_._2 >= flightMinutesRequired) // Only keep usable planes

    val sortedPlanes = airplaneRemainingMinutes.sortBy(-_._2).toBuffer // Prefer planes with most free time

    for (_ <- 0 until airplanesRequired if remainingFrequency > 0) {
      val (selectedPlane, availableMinutes) = if (sortedPlanes.nonEmpty) {
        sortedPlanes.remove(0)
      } else {
        // No existing plane available — buy new
        val newAirplane = createAirplane(model, airline, homeAirport)
        (newAirplane, Airplane.MAX_FLIGHT_MINUTES)
      }

      val frequencyForThis = math.min(remainingFrequency, Math.floor(availableMinutes / flightMinutesRequired).toInt)
      val flightMinutesForThis = frequencyForThis * flightMinutesRequired

      assignedAirplanes.put(selectedPlane, LinkAssignment(frequencyForThis, flightMinutesForThis))

      remainingFrequency -= frequencyForThis
    }

    assignedAirplanes.toMap
  }

  private def createAirplane(model: Model, airline: Airline, homeAirport: Airport): Airplane = {
    val airplane = Airplane(
      model = model,
      owner = airline,
      constructedCycle = 1,
      purchasedCycle = 1,
      condition = Airplane.MAX_CONDITION,
      depreciationRate = 0,
      value = model.price,
      home = homeAirport,
      configuration = AirplaneConfiguration.empty
    )

    airplane.assignDefaultConfiguration()
    AirplaneSource.saveAirplanes(List(airplane))
    airplane
  }
}
