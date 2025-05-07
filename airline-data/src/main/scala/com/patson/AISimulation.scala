package com.patson

import com.patson.data._
import com.patson.model._
import com.patson.model.airplane.Model

/* AI Design

- Route Management
- Pricing
- Finances
- Fleet Management

 */

object AISimulation {
  def simulateAIAirlines(cycle: Int) = {
    val aiAirlines = AirlineSource.loadAllAirlines(true).filterNot(_.aiType == AIType.PLAYER)
    val allLinks = LinkSource.loadAllLinks(LinkSource.FULL_LOAD)



    println(aiAirlines)
  }

  // route management
  // - update pricing on unprofitable/low load factor routes
  // - increase pricing on full load factor routes
  private def findPotentialNewRoute(airline: Airline, airlineFlightLinks: List[Link]) = {

  }

  private def updatePricing(airline: Airline, airlineFlightLinks: List[Link]) = {
    //airlineFlightLinks.forEach(link =>
        //if ()
    //)
  }

  // pricing

  
  // finances


  // fleet management
}
