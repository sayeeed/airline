package com.patson.init

import com.patson.AISimulation

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import com.patson.data.AirportSource
import com.patson.ai.AIAirlines.AirlineStrategyProfile
import com.patson.ai.AIAirlines

object AIDebug extends App {
  private lazy val airports = AirportSource.loadAllAirports(fullLoad = false, loadFeatures = true)
  
  mainFlow
  
  def mainFlow() = {
    //AISimulation.simulateAIAirlines(2)
    val profile = AIAirlines.getAirlineStrategyProfile("Delta Air Lines")

    val primaryToAirports = airports.filter { airport => 
      val notSmallAirport = airport.size > 3
      val isPrimaryCountry = profile.primaryCountriesServed.contains(airport.countryCode)
      val isPrimaryAffinity = airport.zone.split("\\|").map(_.trim).exists(affinity => profile.primaryAffinitiesServed.contains(affinity))
      notSmallAirport && (isPrimaryCountry || isPrimaryAffinity)
    }.distinct.sortBy(airport => (airport.population)).reverse
    val secondaryToAirports = airports.filter { airport => 
      val notSmallAirport = airport.size > 3
      val isSecondaryCountry = profile.secondaryCountriesServed.contains(airport.countryCode)
      val isSecondaryAffinity = airport.zone.split("\\|").map(_.trim).exists(affinity => profile.secondaryAffinitiesServed.contains(affinity))
      notSmallAirport && (isSecondaryCountry || isSecondaryAffinity)
    }.distinct.sortBy(airport => (airport.population)).reverse

    val affinities = List("Sunni")

    

    val affinityAirportTest = airports.filter { airport =>
      val notSmallAirport = airport.size > 3
      val isAffinityMatching = airport.zone.split("\\|").map(_.trim).exists(affinity => affinities.contains(affinity))
      notSmallAirport && isAffinityMatching
    }.distinct.sortBy(airport => (airport.population)).reverse

    //println("PRIMARY AIRPORTS:")
    primaryToAirports.foreach { airport =>
      //println(f"Airport: ${airport.iata}%-6s ${airport.countryCode}%-6s ${airport.city}%-25s ${airport.population}%-10s")
    }

    //println("SECONDARY AIRPORTS:")
    secondaryToAirports.foreach { airport =>
      //println(f"Airport: ${airport.iata}%-6s ${airport.countryCode}%-6s ${airport.city}%-25s ${airport.population}%-10s")
    }

    //println("Testing Affinity Matching:")
    affinityAirportTest.foreach { airport =>
      println(f"Airport: ${airport.iata}%-6s ${airport.countryCode}%-6s ${airport.city}%-25s ${airport.population}%-10s")  
    }

    Await.result(actorSystem.terminate(), Duration.Inf)
  }
}
