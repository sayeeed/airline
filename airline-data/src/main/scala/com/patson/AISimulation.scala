package com.patson

import com.patson.data._
import com.patson.model._

object AISimulation {
  def simulateAIAirlines() = {
    val aiAirlines = AirlineSource.loadAllAirlines(true).filter(_.id < 50)
    println(aiAirlines)
  }

  // route management
  // - update pricing on unprofitable/low load factor routes
  // - increase pricing on full load factor routes
  def routeManagement() = {

  }

  def updatePricing() = {

  }

  // financial management

}
