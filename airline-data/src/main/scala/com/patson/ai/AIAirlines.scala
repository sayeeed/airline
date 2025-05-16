package com.patson.ai

import com.patson.model.AirlineType
import com.patson.model.AIType
import com.patson.model.Airline

object AIAirlines {
	
	private val boeingOldLegacy = List("Boeing 737-800", "Boeing 737-900ER", "Boeing 767-300ER", "Boeing 777-200ER")
  private val boeingNewLegacy = List("Boeing 737-900ER", "Boeing 737 MAX 8", "Boeing 777-300ER", "Boeing 787-9 Dreamliner")

  
  def getAirlineStrategyProfile(airlineName: String) : AirlineStrategyProfile = {
    val airlineProfile = airlineName match {
      // USA
      case "Delta Air Lines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER,
        "ATL", List("JFK","BOS","DTW","LAX","MSP","LGA","SLC","SEA"), 
        List("US"), List(), List("CA","MX","GB","KR","FR"), List("EU","CC"),
        List("US"), List(),
        0.8, 0.2, 500, 15_000, 60, 60,
        List("Boeing 737-800", "Boeing 737-900ER", "Boeing 767-300", "Airbus A350-900"))
      case "United Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER,
        "ORD", List("DEN","IAH","LAX","EWR","SFO","IAD"),
        List("US"), List(), List("US","CA","MX","GB","DE","JP"), List("EU"), 
        List("US"), List(),
        0.8, 0.2, 500, 15_000, 60, 60,
        List("Boeing 737-800", "Boeing 737-900ER", "Boeing 737 MAX 8", "Boeing 767-300", "Boeing 787-9 Dreamliner"))
      case "American Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER,
        "DFW", List("ORD","CLT","LAX","MIA","JFK","LGA","PHL","PHX","DCA"), 
        List("US"), List(), List("US","CA","MX","GB","JP","BR"), List("EU"),
        List("US"), List(),
        0.8, 0.2, 500, 15_000, 60, 60,
        List("Boeing 737-800", "Boeing 737 MAX 8", "Boeing 777-200", "Boeing 787-9 Dreamliner"))
      /*case "Southwest Airlines" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, "DAL", List("MDW","DEN","LAS","BWI","PHX","HOU","ATL","MCO"), true, false, List("US","MX"), List("Anglophone","CC"), 1.0, 0.0, 0.0, 200, 40, 6_000, 32, 4, 40, List("Boeing 737-700", "Boeing 737-800", "Boeing 737 MAX 8"))
      case "JetBlue Airways" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, "JFK", List("BOS","FLL","MCO","LAX","SJU"), true, false, List("US","MX","GB"), List("Anglophone","CC"), 1.0, 0.0, 0.0, 200, 40, 8_000, 28, 6, 40, List("Airbus A320-200", "Airbus A321", "Airbus A321neo"))
      case "Alaska Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, "SEA", List("ANC","PDX","SFO","LAX","SAN"), true, false, List("US","MX","CA","JP"), List("Anglophone"), 1.0, 0.0, 0.0, 300, 40, 10_000, 28, 6, 50, List("Boeing 737-900ER", "Boeing 737-800", "Boeing 737 MAX 9"))
      case "Spirit Airlines" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, "FLL", List("MCO","DTW","LAS","DFW","ACY"), true, false, List("US","MX"), List("Anglophone","CC"), 1.0, 0.0, 0.0, 200, 20, 6_000, 32, 4, 35, List("Airbus A320", "Airbus A321", "Airbus A319"))
      case "Frontier Airlines" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, "DEN", List("MCO","LAS","PHL","MIA","ATL"), true, false, List("US","MX"), List("Anglophone","CC"), 1.0, 0.0, 0.0, 200, 20, 6_000, 32, 4, 35, List("Airbus A319","Airbus A321neo","Airbus A320neo"))
      case "Hawaiian Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, "HNL", List("OGG","LAX","SFO","SEA"), true, false, List("US","JP","AU","KR"), List("Anglophone"), 1.0, 0.0, 0.0, 300, 60, 14_000, 24, 8, 60, List("Airbus A321neo","Airbus A330-200","Boeing 717-200"))
      case "Allegiant Air" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, "LAS", List("SFB","PIE","AZA","CVG","IND"), true, false, List("US","MX"), List("Anglophone"), 1.0, 0.0, 0.0, 200, 20, 6_000, 32, 4, 35, List("Airbus A319","Airbus A320"))
      // Canada
      case "Air Canada" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, "YYZ", List("YUL","YVR","YYC","YHZ","YOW"), true, false, List("US","CA","MX","GB","DE","FR","CN"), List("Anglophone", "CC"), 1.0, 0.0, 0.0, 500, 60, 14_000, 24, 8, 60, List("Boeing 787-9 Dreamliner", "Boeing 777-300ER", "Airbus A320-200", "Airbus A321neo", "Boeing 737 MAX 8"))
      case "WestJet" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, "YYC", List("YYZ","YVR"), true, false, List("US","CA","MX","GB","IE","FR","IT"), List("Anglophone", "CC"), 1.0, 0.0, 0.0, 200, 60, 14_000, 24, 8, 50, List("Boeing 737-800", "Boeing 737 MAX 8", "Boeing 787-9 Dreamliner"))
      case "Porter Airlines" => AirlineStrategyProfile(AirlineType.REGIONAL, AIType.PASSIVE, "YTZ", List("YYZ","YOW","YHZ","YUL"), true, false, List("US","CA"), List("Anglophone"), 1.0, 0.0, 0.0, 50, 40, 4_000, 40, 0, 40, List("Embraer E195-E2", "De Havilland DHC-8-400"))
      // Mexico
      case "Aeromexico" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, "MEX", List("GDL","MTY"), false, true, List("US","CA","MX","CO","AR","BR","ES","FR","GB","JP","KR"), List("Hispanic"), 0.8, 0.2, 0.0, 500, 60, 14_000, 24, 8, 60, List("Boeing 787-9 Dreamliner", "Boeing 777-300ER", "Airbus A320-200", "Airbus A321neo", "Boeing 737 MAX 8"))
      */

      // Middle East
      case "Emirates" => AirlineStrategyProfile(AirlineType.MEGA_HQ, AIType.PASSIVE, AirlineFocusStrategy.GLOBAL_PRESTIGE_CARRIER,
        "DXB", List(),
        List("IN","CN","JP","KR","AU","BR","AG","US","GB"), List("EU","ASEAN"), List(), List("Sunni"),
        List(), List(),
        0.9, 0.1, 500, 16_000, 80, 70,
        List("Boeing 777-300ER","Airbus A380-800"))
    }
    airlineProfile
  }

	case class AirlineStrategyProfile(
    airlineType: AirlineType.Value,
    aiType: AIType.Value,
    focusStrategy: AirlineFocusStrategy.Value,
    hqAirport: String,
    bases: List[String],
    primaryCountriesServed: List[String],
    primaryAffinitiesServed: List[String],
    secondaryCountriesServed: List[String],
    secondaryAffinitiesServed: List[String],
    primaryBaseCountries: List[String],
    primaryBaseAffinities: List[String],
    primaryServedRatio: Double,
    secondaryServedRatio: Double,
    minimumRouteDemand: Int,
    linkMaxDistance: Int,
    routeServiceLevel: Int,
    targetServiceQuality: Int,
    modelNames: List[String]
  )

  object AirlineFocusStrategy extends Enumeration {
    type AirlineFocusStrategy = Value
    val FLAG_CARRIER, LOW_COST_DOMESTIC_CARRIER, REGIONAL_CARRIER, GLOBAL_PRESTIGE_CARRIER = Value
    val label: AirlineFocusStrategy => String = {
      case FLAG_CARRIER => "Flag Carrier"
      case LOW_COST_DOMESTIC_CARRIER => "Low-Cost Domestic Carrier"
      case REGIONAL_CARRIER => "Regional Carrier"
      case GLOBAL_PRESTIGE_CARRIER => "Global Prestige Carrier"
    }
    def fromId(id: Int): AirlineFocusStrategy = id match {
      case 0 => FLAG_CARRIER
      case 1 => LOW_COST_DOMESTIC_CARRIER
      case 2 => REGIONAL_CARRIER
      case 3 => GLOBAL_PRESTIGE_CARRIER
      case _ => throw new IllegalArgumentException("Invalid AirlineFocusStrategy ID: " + id)
    }
  }
}
