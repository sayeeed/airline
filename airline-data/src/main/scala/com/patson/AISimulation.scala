package com.patson

import com.patson.data._
import com.patson.model._
import com.patson.model.airplane.Model
import com.patson.model.airplane.AirplaneConfiguration.first
import scala.util.Random

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
          // all 10-week averages
          val (economyLF, businessLF, firstLF, totalLF) = computeAverageLoadFactor(flightLink, cycle)
            
          // adjusts prices down if total load factor is below 90% and no major delays or cancellations occurred
          if (totalLF < 90 && linkConsumption.link.majorDelayCount == 0 && linkConsumption.link.cancellationCount == 0) {
            adjustPrices(flightLink, linkConsumption)
          }
          // adjusts price up if one of the seat classes has 100% LF
          if (economyLF == 100 || businessLF == 100 || firstLF == 100) {
            adjustPrices(flightLink, linkConsumption)
          }
        }
    }
  }

  /* 
   * Adjusts the price (economy, business, first all separated) of the specified flight link by 5%:
   *  - Down if the respective load factor is below a certain percentage (e.g. 90) and the current 
   *    percentage off base price is no less than 75% (to prevent prices spiraling down)
   *  - Up if the respective load factor is equal to 100%
   */
  private def adjustPrices(flightLink: Link, linkConsumption: LinkConsumptionDetails) = {
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

    val newPrices = LinkClassValues(newEconomyPrice.toInt, newBusinessPrice.toInt, newFirstPrice.toInt)
    val newLink = flightLink.copy(price = newPrices)
    println("Updated the above link to new prices: " + newLink)
    LinkSource.updateLink(newLink)
  }

  private def adjustFrequency(flightLink: Link, linkConsumption: LinkConsumptionDetails, rivalFlightLinks: List[Link]) = {
    //lazy val countryRelationships = CountrySource.getCountryMutualRelationships()
    //val relationship = countryRelationships.getOrElse((flightLink.from.countryCode, flightLink.to.countryCode), 0)
    //val affinity = Computation.calculateAffinityValue(flightLink.from.zone, flightLink.to.zone, relationship)
    //val demand = DemandGenerator.computeBaseDemandBetweenAirports(flightLink.from, flightLink.to, affinity, flightLink.distance)
    var linkTotalCapacity = flightLink.getTotalCapacity
    var linkTotalSoldSeats = flightLink.getTotalSoldSeats

    rivalFlightLinks.foreach {
      rivalFlightLink =>
        linkTotalCapacity += rivalFlightLink.getTotalCapacity
        linkTotalSoldSeats += rivalFlightLink.getTotalSoldSeats
    }

    if (linkTotalSoldSeats.toDouble / linkTotalCapacity >= 0.90) {

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

  // finances


  // fleet management
}
