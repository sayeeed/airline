package com.patson

import com.patson.data._
import com.patson.model._
import com.patson.model.airplane.Model
import com.patson.model.airplane.AirplaneConfiguration.first

/* AI Design

- Route Management
- Pricing
- Finances
- Fleet Management

 */

object AISimulation {
  def simulateAIAirlines(cycle: Int) = {
    val aiAirlines = AirlineSource.loadAllAirlines(true).filterNot(_.aiType == AIType.PLAYER)
    val allFlightLinks = LinkSource.loadAllFlightLinks()
    val flightLinksByAirline = allFlightLinks.groupBy(_.airline.id)

    aiAirlines.foreach {
      case airline : Airline =>
        val airlineFlightLinks = flightLinksByAirline.getOrElse(airline.id, Nil)
        updatePricing(airline, airlineFlightLinks, cycle)
    }

    println(aiAirlines)
  }

  // route management
  // - update pricing on unprofitable/low load factor routes
  // - increase pricing on full load factor routes
  private def updatePricing(airline: Airline, flightLinks: List[Link], cycle: Int) = {
    flightLinks.foreach {
      case flightLink : Link =>
        //println("Updating pricing for Airline: " + flightLink.airline.name + " Link: " + flightLink.from.iata + "-" + flightLink.to.iata)
        val linkConsumptions = LinkSource.loadLinkConsumptionsByLinkId(flightLink.id, cycle).headOption
        val rivalFlightLinks = LinkSource.loadFlightLinksByAirports(flightLink.from.id, flightLink.to.id).filterNot(_.airline.id == flightLink.airline.id)

        linkConsumptions.foreach {
          linkConsumption =>
            // if load factor is below a certain percentage, adjust price
            val loadFactor = ((linkConsumption.link.getTotalSoldSeats.toDouble / linkConsumption.link.getTotalCapacity.toDouble) * 100).toInt
            
            if (loadFactor < 90 && linkConsumption.link.majorDelayCount == 0 && linkConsumption.link.cancellationCount == 0) {
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

  
  // finances


  // fleet management
}
