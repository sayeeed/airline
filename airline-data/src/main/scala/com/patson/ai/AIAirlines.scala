package com.patson.ai

import com.patson.model.AirlineType
import com.patson.model.AIType

object AIAirlines {
	
	def getAirlineStrategyProfile(airlineName: String) : AirlineStrategyProfile = {
    val airlineProfile = airlineName match {
      // North America
      case "Delta Air Lines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, "ATL", List("JFK","BOS","DTW","LAX","MSP","LGA","SLC","SEA"), true, List("US","CA","MX","UK","SK","FR"), List("Anglophone"), 1.0, 0.0, 0.0, 500, 60, 14_000, 24, 8, 60, List("Boeing 737-800", "Boeing 737-900ER", "Boeing 767-300", "Airbus A350-900"))
      case "United Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, "ORD", List("DEN","GUM","IAH","LAX","EWR","SFO","IAD"), true, List("US","CA","MX","UK","GE","JP"), List("Anglophone"), 1.0, 0.0, 0.0, 500, 60, 14_000, 24, 8, 60, List("Boeing 737-800", "Boeing 737-900ER", "Boeing 737 MAX 8", "Boeing 767-300", "Boeing 787-9 Dreamliner"))
      case "American Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, "DFW", List("ORD","CLT","LAX","MIA","JFK","LGA","PHL","PHX","DCA"), true, List("US","CA","MX","UK","JP","BR"), List("Anglophone"), 1.0, 0.0, 0.0, 500, 60, 14_000, 24, 8, 60, List("Boeing 737-800", "Boeing 737 MAX 8", "Boeing 777-200", "Boeing 787-9 Dreamliner"))
      case "Southwest Airlines" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, "DAL", List("MDW","DEN","LAS","BWI","PHX","HOU","ATL","MCO"), true, List("US","MX"), List("Anglophone","CC"), 1.0, 0.0, 0.0, 200, 40, 6_000, 32, 4, 40, List("Boeing 737-700", "Boeing 737-800", "Boeing 737 MAX 8"))
      case "JetBlue Airways" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, "JFK", List("BOS","FLL","MCO","LAX","SJU"), true, List("US","MX","UK"), List("Anglophone","CC"), 1.0, 0.0, 0.0, 200, 40, 8_000, 28, 6, 40, List("Airbus A320-200", "Airbus A321", "Airbus A321neo"))
      case "Alaska Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, "SEA", List("ANC","PDX","SFO","LAX","SAN"), true, List("US","MX","CA","JP"), List("Anglophone"), 1.0, 0.0, 0.0, 300, 40, 10_000, 28, 6, 50, List("Boeing 737-900ER", "Boeing 737-800", "Boeing 737 MAX 9"))
      case "Spirit Airlines" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, "FLL", List("MCO","DTW","LAS","DFW","ACY"), true, List("US","MX"), List("Anglophone","CC"), 1.0, 0.0, 0.0, 200, 20, 6_000, 32, 4, 35, List("Airbus A320", "Airbus A321", "Airbus A319"))
      case "Frontier Airlines" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, "DEN", List("MCO","LAS","PHL","MIA","ATL"), true, List("US","MX"), List("Anglophone","CC"), 1.0, 0.0, 0.0, 200, 20, 6_000, 32, 4, 35, List("Airbus A319","Airbus A321neo","Airbus A320neo"))
      case "Hawaiian Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, "HNL", List("OGG","LAX","SFO","SEA"), true, List("US","JP","AU","SK"), List("Anglophone"), 1.0, 0.0, 0.0, 300, 60, 14_000, 24, 8, 60, List("Airbus A321neo","Airbus A330-200","Boeing 717-200"))
      case "Allegiant Air" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, "LAS", List("SFB","PIE","AZA","CVG","IND"), true, List("US","MX"), List("Anglophone"), 1.0, 0.0, 0.0, 200, 20, 6_000, 32, 4, 35, List("Airbus A319","Airbus A320"))
    }
    airlineProfile
  }

	case class AirlineStrategyProfile(
    airlineType: AirlineType.Value,
    aiType: AIType.Value,
    hqAirport: String,
    bases: List[String],
    preferredDomestic: Boolean,
    preferredCountries: List[String],
    preferredAffinities: List[String],
    domesticBasePriority: Double,
    regionBasePriority: Double,
    globalBasePriority: Double,
    minimumRouteDemand: Int,
    routeServiceLevel: Int,
    linkMaxDistance: Int,
    maxLinksPerBase: Int,
    maxLongLinksPerBase: Int,
    targetServiceQuality: Int,
    modelNames: List[String]
  )
}
