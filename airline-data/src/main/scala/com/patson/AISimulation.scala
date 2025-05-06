package com.patson

import com.patson.data._
import com.patson.model._

object AISimulation {
  def simulateAIAirlines(cycle: Int) = {
    //val aiAirlines = AirlineSource.loadAllAirlines(true).filter(_.id < 50)
    //val allLinks = LinkSource.loadAllLinks(LinkSource.FULL_LOAD)

    //println(aiAirlines)
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

  // financial management
}
