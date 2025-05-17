package com.patson.ai

import com.patson.model.AirlineType
import com.patson.model.AIType
import com.patson.model.Airline
import com.patson.init.AirlineGenerator.AIAlliance

object AIAirlines {
	
	private val boeingOldLegacy = List("Boeing 737-800", "Boeing 737-900ER", "Boeing 767-300ER", "Boeing 777-200ER")
  private val boeingNewLegacy = List("Boeing 737-900ER", "Boeing 737 MAX 8", "Boeing 777-300ER", "Boeing 787-9 Dreamliner")

  
  def getAirlineStrategyProfile(airlineName: String) : AirlineStrategyProfile = {
    val airlineProfile = airlineName match {
      // USA
      case "Delta Air Lines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.SKYTEAM,
        "ATL", List("JFK","BOS","DTW","LAX","MSP","LGA","SLC","SEA"), 
        List("US"), List(), List("CA","MX","GB","KR","FR"), List("EU","CC"),
        List("US"), List(),
        0.8, 0.2, 500, 15_000, 60, 60,
        List("Boeing 737-800", "Boeing 737-900ER", "Boeing 767-300", "Airbus A350-900"))
      case "United Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.STAR_ALLIANCE,
        "ORD", List("DEN","IAH","LAX","EWR","SFO","IAD"),
        List("US"), List(), List("CA","MX","GB","DE","JP"), List("EU"), 
        List("US"), List(),
        0.8, 0.2, 500, 15_000, 60, 60,
        List("Boeing 737-900ER", "Boeing 737 MAX 8", "Boeing 767-300", "Boeing 787-9 Dreamliner"))
      case "American Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.ONEWORLD,
        "DFW", List("ORD","CLT","LAX","MIA","JFK","LGA","PHL","PHX","DCA"), 
        List("US"), List(), List("CA","MX","GB","JP","BR"), List("EU"),
        List("US"), List(),
        0.8, 0.2, 500, 15_000, 60, 60,
        List("Boeing 737-800", "Boeing 737 MAX 8", "Boeing 777-200", "Boeing 787-9 Dreamliner"))
      case "Southwest Airlines" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, AirlineFocusStrategy.LOW_COST_DOMESTIC_CARRIER, AIAlliance.SKYTEAM,
        "DAL", List("MDW","DEN","LAS","BWI","PHX","HOU","ATL","MCO"),
        List("US"), List(), List("CA","MX"), List("CC"),
        List("US"), List(),
        0.8, 0.2, 200, 6_000, 40, 40,
        List("Boeing 737-700", "Boeing 737 MAX 8"))
      case "JetBlue Airways" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.AGGRESSIVE, AirlineFocusStrategy.LOW_COST_DOMESTIC_CARRIER, AIAlliance.SKYTEAM,
        "JFK", List("BOS","FLL","MCO","LAX","SJU"),
        List("US"), List(), List("CA","MX","GB"), List("CC"),
        List("US"), List(),
        0.8, 0.2, 200, 6_000, 40, 40,
        List("Airbus A320-200","Airbus A321neo"))
      case "Alaska Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.LEGACY_CARRIER, AIAlliance.SKYTEAM,
        "SEA", List("ANC","PDX","SFO","LAX","SAN"),
        List("US"), List(), List("CA","MX","JP"), List(),
        List("US"), List(),
        0.8, 0.2, 300, 8_000, 60, 50,
        List("Boeing 737-900ER","Boeing 737 MAX 9"))
      case "Delta Connection" => AirlineStrategyProfile(AirlineType.REGIONAL, AIType.PASSIVE, AirlineFocusStrategy.REGIONAL_CARRIER, AIAlliance.SKYTEAM,
        "ATL", List("JFK","BOS","DTW","LAX","MSP","LGA","SLC","SEA"), 
        List("US"), List(), List(), List(),
        List("US"), List(),
        1.0, 0.0, 50, 3_000, 60, 50,
        List("Bombardier CRJ900"))
      case "United Express" => AirlineStrategyProfile(AirlineType.REGIONAL, AIType.PASSIVE, AirlineFocusStrategy.REGIONAL_CARRIER, AIAlliance.STAR_ALLIANCE,
        "ORD", List("DEN","IAH","LAX","EWR","SFO","IAD"),
        List("US"), List(), List(), List(), 
        List("US"), List(),
        1.0, 0.0, 50, 3_000, 60, 50,
        List("Embraer E175-E2"))
      case "American Eagle" => AirlineStrategyProfile(AirlineType.REGIONAL, AIType.PASSIVE, AirlineFocusStrategy.REGIONAL_CARRIER, AIAlliance.ONEWORLD,
        "DFW", List("ORD","CLT","LAX","MIA","JFK","LGA","PHL","PHX","DCA"), 
        List("US"), List(), List(), List(),
        List("US"), List(),
        1.0, 0.0, 50, 3_000, 60, 50,
        List("Embraer E175-E2"))
      
      // Canada
      case "Air Canada" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.STAR_ALLIANCE,
        "YYZ", List("YUL","YVR","YYC","YHZ","YOW"),
        List("CA","US"), List(), List("MX","GB","DE","FR","JP","CN"), List("CC"),
        List("US"), List(),
        0.8, 0.2, 500, 15_000, 60, 60,
        List("Boeing 737 MAX 8", "Boeing 787-9 Dreamliner"))
      case "WestJet" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.LEGACY_CARRIER, AIAlliance.PLACEHOLDER,
        "YYC", List("YYZ","YVR"),
        List("CA","US"), List(), List("MX","GB","IE","FR","IT"), List("CC"),
        List("CA"), List(),
        0.8, 0.2, 300, 10_000, 60, 50,
        List("Boeing 737-800", "Boeing 737 MAX 8", "Boeing 787-9 Dreamliner"))
      case "Porter Airlines" => AirlineStrategyProfile(AirlineType.REGIONAL, AIType.PASSIVE, AirlineFocusStrategy.REGIONAL_CARRIER, AIAlliance.PLACEHOLDER,
        "YTZ", List("YYZ","YOW","YHZ","YUL"),
        List("CA"), List(), List("US"), List(),
        List("CA"), List(),
        0.6, 0.4, 50, 4_000, 40, 40,
        List("Embraer E195-E2", "De Havilland DHC-8-400"))
      
      // Mexico
      case "Aeromexico" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.SKYTEAM,
        "MEX", List("GDL","MTY"),
        List("MX"), List(), List("US","CA","CO","AR","BR","ES","JP","KR"), List("Hispanic","EU"),
        List("MX"), List(),
        0.5, 0.5, 500, 15_000, 60, 50,
        List("Boeing 787-9 Dreamliner", "Boeing 777-300ER", "Airbus A320-200", "Airbus A321neo"))
      case "Volaris" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.PASSIVE, AirlineFocusStrategy.LOW_COST_DOMESTIC_CARRIER, AIAlliance.PLACEHOLDER,
        "MEX", List("CUN","CUL","GDL","BJX","MXL","MTY","TIJ"),
        List("MX"), List(), List("US","CO","AR","BR"), List("Hispanic","CC"),
        List("MX"), List(),
        0.5, 0.5, 300, 8_000, 40, 40,
        List("Airbus A320neo", "Airbus A321neo"))

      // Central America
      case "Copa Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "PTY", List("SJO","SAL","GUA","MEX","CUN","BOG"),
        List(), List("Hispanic"), List("US"), List("CC"),
        List(), List("Hispanic"),
        0.7, 0.3, 200, 8_000, 40, 50,
        List("Boeing 737-800", "Boeing 737 MAX 9"))
      case "Avianca El Salvador" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "SAL", List("SJO","GUA","PTY","MEX"),
        List(), List("Hispanic"), List("US"), List("CC"),
        List(), List("Hispanic"),
        0.7, 0.3, 200, 8_000, 40, 50,
        List("Airbus A319", "Airbus A320"))

      // South America
      case "LATAM Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "SCL", List("GRU","LIM","BOG","UIO","EZE"),
        List("CL","BR"), List("Hispanic"), List("US","GB","JP","KR","AU","NZ"), List("EU","ASEAN"),
        List("BR"), List("Hispanic"),
        0.7, 0.3, 500, 15_000, 60, 50,
        List("Airbus A320neo", "Airbus A321neo", "Boeing 777-300ER","Boeing 787-9 Dreamliner"))
      case "Avianca" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "BOG", List("MDE","CLO","CTG","SJO","UIO"),
        List("CO","BR"), List("Hispanic"), List("US"), List("CC"),
        List("BR"), List("Hispanic"),
        0.7, 0.3, 500, 8_000, 60, 50,
        List("Airbus A320neo","Boeing 777-300ER"))
      case "Azul Linhas Aereas" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.PASSIVE, AirlineFocusStrategy.LOW_COST_DOMESTIC_CARRIER, AIAlliance.PLACEHOLDER,
        "VCP", List("CNF","REC","BEL","POA","CGB"),
        List("BR"), List("Hispanic"), List("US"), List("EU"),
        List("BR"), List("Hispanic"),
        0.8, 0.2, 300, 12_000, 40, 40,
        List("Airbus A320neo","Airbus A330-200"))
      case "Gol Linhas Aereas" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.PASSIVE, AirlineFocusStrategy.LOW_COST_DOMESTIC_CARRIER, AIAlliance.PLACEHOLDER,
        "GRU", List("GIG","BSB","SSA","FOR","REC"),
        List("BR"), List("Hispanic"), List("US"), List("CC"),
        List("BR"), List("Hispanic"),
        0.8, 0.2, 100, 8_000, 40, 40,
        List("Boeing 737-700", "Boeing 737 MAX 8"))
      case "Aerolineas Argentinas" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "EZE", List("COR","MDZ","ROS","SLA","BRC"),
        List("AR","BR"), List("Hispanic"), List("US"), List("EU"),
        List("AR"), List("Hispanic"),
        0.8, 0.2, 100, 12_000, 60, 50,
        List("Boeing 737-700", "Airbus A330-200"))
      
      // Europe
      case "Ryanair" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.PASSIVE, AirlineFocusStrategy.LOW_COST_DOMESTIC_CARRIER, AIAlliance.PLACEHOLDER,
        "DUB", List("STN","BGY","MAD","CRL","BER"),
        List("GB"), List("EU"), List(), List(),
        List("GB"), List("EU"),
        1.0, 0.0, 200, 5_000, 40, 40,
        List("Boeing 737-800","Boeing 737 MAX 8"))
      case "Lufthansa" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "FRA", List("MUC","DUS","HAM","BER","ZRH"),
        List("DE","GB"), List("EU"), List("US","JP","KR","CN"), List("ASEAN","Hispanic","Arabic"),
        List(), List("EU"),
        0.7, 0.3, 500, 15_000, 60, 60,
        List("Airbus A320neo", "Airbus A330-800neo", "Airbus A350-1000"))
      case "Air France" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "CDG", List("ORY","LYS","MRS","TLS","NCE"),
        List("FR","GB"), List("EU"), List("US","JP","KR","CN","CA"), List("ASEAN","Hispanic","Francophonie","Arabic"),
        List(), List("EU"),
        0.7, 0.3, 500, 15_000, 60, 60,
        List("Airbus A320neo", "Airbus A330-800neo", "Airbus A350-1000"))
      case "KLM" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "AMS", List("RTM","EIN","GRQ","MST","BRU"),
        List("GB"), List("EU"), List("US","CA","JP","KR","CN"), List("ASEAN","Hispanic","Arabic"),
        List(), List("EU"),
        0.7, 0.3, 500, 15_000, 60, 60,
        List("Boeing 737-900ER","Boeing 777-300ER","Boeing 787-9 Dreamliner"))
      case "British Airways" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "LHR", List("LGW","MAN","EDI","GLA","BHD"),
        List("GB"), List("EU"), List("US","CA","JP","KR","CN"), List("ASEAN","Hispanic","Arabic"),
        List("GB"), List("EU"),
        0.7, 0.3, 500, 15_000, 60, 60,
        List("Airbus A320neo","Airbus A350-900","Boeing 787-9 Dreamliner"))
      case "Turkish Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "IST", List("ESB","ADB","AYT","TZX","ADA"),
        List("GB","TR"), List("EU","Arabic"), List("US","CA","JP","KR","CN"), List("ASEAN","Hispanic"),
        List("TR"), List("Arabic"),
        0.7, 0.3, 500, 15_000, 60, 60,
        List("Airbus A320neo", "Airbus A330-200", "Airbus A350-900"))
      case "easyJet" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.PASSIVE, AirlineFocusStrategy.LOW_COST_DOMESTIC_CARRIER, AIAlliance.PLACEHOLDER,
        "LTN", List("LGW","MAN","BRS","EDI","MXP"),
        List("GB"), List("EU"), List(), List(),
        List("GB"), List("EU"),
        1.0, 0.0, 200, 5_000, 40, 40,
        List("Airbus A319", "Airbus A321neo"))

      // Middle East
      case "Emirates" => AirlineStrategyProfile(AirlineType.MEGA_HQ, AIType.PASSIVE, AirlineFocusStrategy.GLOBAL_PRESTIGE_CARRIER, AIAlliance.PLACEHOLDER,
        "DXB", List(),
        List("IN","CN","JP","KR","AU","BR","US","GB"), List("EU","ASEAN","Hispanic"), List(), List("Sunni","Arabic"),
        List(), List(),
        0.9, 0.1, 500, 16_000, 80, 70,
        List("Boeing 777-300ER","Airbus A380-800"))
      case "Qatar Airways" => AirlineStrategyProfile(AirlineType.MEGA_HQ, AIType.PASSIVE, AirlineFocusStrategy.GLOBAL_PRESTIGE_CARRIER, AIAlliance.PLACEHOLDER,
        "DOH", List(),
        List("IN","CN","JP","KR","AU","BR","US","GB"), List("EU","ASEAN","Hispanic"), List(), List("Sunni","Arabic"),
        List(), List(),
        0.9, 0.1, 500, 16_000, 80, 70,
        List("Airbus A320neo","Airbus A350-1000"))
      case "Etihad Airways" => AirlineStrategyProfile(AirlineType.MEGA_HQ, AIType.PASSIVE, AirlineFocusStrategy.GLOBAL_PRESTIGE_CARRIER, AIAlliance.PLACEHOLDER,
        "AUH", List(),
        List("IN","CN","JP","KR","AU","BR","US","GB"), List("EU","ASEAN","Hispanic"), List(), List("Sunni","Arabic"),
        List(), List(),
        0.9, 0.1, 500, 16_000, 80, 70,
        List("Airbus A320neo","Airbus A350-1000"))
      case "Saudia" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "JED", List("RUH","DMM","MED"),
        List("IN","CN","JP","KR","AU","BR","US","GB"), List("EU","ASEAN","Hispanic"), List(), List("Sunni","Arabic"),
        List(), List("Arabic","Sunni"),
        0.7, 0.3, 500, 16_000, 80, 70,
        List("Airbus A320neo","Airbus A350-1000"))

      // India
      case "IndiGo" => AirlineStrategyProfile(AirlineType.DISCOUNT, AIType.PASSIVE, AirlineFocusStrategy.LOW_COST_DOMESTIC_CARRIER, AIAlliance.PLACEHOLDER,
        "DEL", List("BOM","BLR","HYD","CCU","MAA"),
        List("IN"), List(), List(), List("ASEAN","Indian diaspora"),
        List("IN"), List(),
        0.8, 0.2, 200, 6_000, 20, 25,
        List("Airbus A320neo", "Airbus A321neo"))
      case "Air India" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "DEL", List("BOM","BLR","HYD","CCU","MAA"),
        List("IN"), List(), List("US","JP","KR"), List("ASEAN","Indian diaspora","EU"),
        List("IN"), List(),
        0.7, 0.3, 500, 15_000, 40, 40,
        List("Airbus A320neo", "Airbus A321neo","Airbus A350-900"))

      // China
      case "Air China" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "PEK", List("PKX","CTU","TFU","PVG"),
        List("CN","HK"), List(), List("US","JP","KR","AU"), List("ASEAN","EU"),
        List("CN","HK"), List(),
        0.7, 0.3, 500, 15_000, 60, 50,
        List("Airbus A320", "Airbus A330-200", "Airbus A350-900"))
      case "China Southern Airlines" => AirlineStrategyProfile(AirlineType.MEGA_HQ, AIType.PASSIVE, AirlineFocusStrategy.LEGACY_CARRIER, AIAlliance.PLACEHOLDER,
        "CAN", List("PKX"),
        List("CN","HK"), List(), List("US","JP","KR","AU"), List("ASEAN","EU","Arabic"),
        List(), List(),
        0.7, 0.3, 500, 15_000, 60, 50,
        List("Airbus A320", "Airbus A330-200", "Airbus A350-900"))
      case "China Eastern Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.LEGACY_CARRIER, AIAlliance.PLACEHOLDER,
        "PVG", List("SHA","KMG","XIY","NKG","WUH"),
        List("CN","HK"), List(), List("US","JP","KR","AU"), List("ASEAN","EU","Arabic"),
        List("CN","HK"), List(),
        0.7, 0.3, 500, 15_000, 60, 50,
        List("Airbus A320", "Airbus A330-200", "Airbus A350-900"))

      // Asia
      case "Cathay Pacific" => AirlineStrategyProfile(AirlineType.MEGA_HQ, AIType.PASSIVE, AirlineFocusStrategy.GLOBAL_PRESTIGE_CARRIER, AIAlliance.PLACEHOLDER,
        "HKG", List(),
        List("CN","HK","US","JP","KR","AU","IN"), List("ASEAN","EU","Arabic"), List(), List(),
        List(), List(),
        1.0, 0.0, 500, 15_000, 60, 60,
        List("Airbus A350-1000", "Boeing 777-300ER"))
      case "Singapore Airlines" => AirlineStrategyProfile(AirlineType.MEGA_HQ, AIType.PASSIVE, AirlineFocusStrategy.GLOBAL_PRESTIGE_CARRIER, AIAlliance.PLACEHOLDER,
        "SIN", List(),
        List("CN","HK","US","JP","KR","AU","IN"), List("ASEAN","EU","Arabic"), List(), List(),
        List(), List(),
        1.0, 0.0, 500, 15_000, 60, 60,
        List("Airbus A350-1000", "Boeing 777-300ER"))
      case "All Nippon Airways" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.LEGACY_CARRIER, AIAlliance.PLACEHOLDER,
        "HND", List("NRT","ITM","KIX"),
        List("CN","KR","US","CA","JP"), List("ASEAN"), List("GB","AU","NZ"), List("EU"),
        List("JP","KR"), List("ASEAN"),
        0.7, 0.3, 500, 15_000, 60, 60,
        List("Airbus A320neo","Airbus A380-800","Boeing 777-300ER"))
      case "Japan Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.LEGACY_CARRIER, AIAlliance.PLACEHOLDER,
        "HND", List("NRT","ITM","KIX"),
        List("CN","KR","US","CA","JP"), List("ASEAN"), List("GB","AU","NZ"), List("EU"),
        List("JP","KR"), List("ASEAN"),
        0.7, 0.3, 500, 15_000, 60, 60,
        List("Airbus A321neo","Airbus A350-900","Boeing 777-300ER"))
      case "Korean Air" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.LEGACY_CARRIER, AIAlliance.PLACEHOLDER,
        "ICN", List("GMP","PUS","CJU"),
        List("CN","KR","US","CA","JP"), List("ASEAN"), List("GB","AU","NZ"), List("EU"),
        List("JP","KR"), List("ASEAN"),
        0.7, 0.3, 500, 15_000, 60, 60,
        List("Airbus A321neo","Airbus A350-900","Boeing 777-300ER"))

      // Southeast Asia
      case "Malaysia Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "KUL", List("BKI","PEN"),
        List("CN","KR","JP"), List("ASEAN"), List("GB","AU","NZ","IN"), List("EU","Arabic"),
        List(), List("ASEAN"),
        0.7, 0.3, 500, 12_000, 60, 50,
        List("Airbus A320","Airbus A330-300","Airbus A350-900"))
      case "Garuda Indonesia" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "CGK", List("DPS","SUB","KNO"),
        List("CN","KR","JP"), List("ASEAN"), List("GB","AU","NZ","IN"), List("EU","Arabic"),
        List(), List("ASEAN"),
        0.7, 0.3, 500, 12_000, 60, 50,
        List("Airbus A320","Airbus A330-300","Boeing 777-300ER"))
      case "Phillipine Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "MNL", List("CEB","DVO"),
        List("CN","KR","JP"), List("ASEAN"), List("GB","AU","NZ","IN","US"), List("EU","Arabic"),
        List(), List("ASEAN"),
        0.7, 0.3, 500, 12_000, 60, 50,
        List("Airbus A320","Airbus A330-300","Boeing 777-300ER"))
      case "Vietnam Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "HAN", List("SGN","DAD"),
        List("CN","KR","JP"), List("ASEAN"), List("GB","AU","NZ","IN","US"), List("EU","Arabic"),
        List(), List("ASEAN"),
        0.7, 0.3, 500, 12_000, 60, 50,
        List("Airbus A320","Airbus A350-900","Boeing 787-9 Dreamliner"))
      case "Thai Airways" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "BKK", List("CNX","HKT"),
        List("CN","KR","JP"), List("ASEAN"), List("GB","AU","NZ","IN"), List("EU","Arabic"),
        List(), List("ASEAN"),
        0.7, 0.3, 500, 12_000, 60, 50,
        List("Airbus A320","Airbus A350-900","Boeing 787-9 Dreamliner"))
      
      // Australia
      case "Qantas" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "SYD", List("MEL","BNE","PER","ADL"),
        List("CN","KR","JP","US","CA","GB","AU","NZ"), List("ASEAN","EU"), List(), List("Arabic"),
        List("AU","NZ"), List(),
        0.7, 0.3, 500, 15_000, 60, 60,
        List("Airbus A320neo","Airbus A330-300","Airbus A350-1000"))
      
      // Africa
      case "Ethiopian Airlines" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "ADD", List(),
        List("ET","NG","EG","CD","TZ","ZA","KE","UG","DZ","GH"), List(), List("IN"), List("Arabic","EU"),
        List("ET","NG","EG","CD","TZ","ZA","KE","UG","DZ","GH"), List(),
        0.6, 0.4, 200, 12_000, 20, 40,
        List("Boeing 737-700","Boing 777-300ER","Boeing 787-9 Dreamliner"))
      case "South African Airways" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "JNB", List("CPT","DUR"),
        List("ET","NG","EG","CD","TZ","ZA","KE","UG","DZ","GH"), List(), List("IN","GB"), List("Arabic","EU"),
        List("ET","NG","EG","CD","TZ","ZA","KE","UG","DZ","GH"), List(),
        0.6, 0.4, 200, 12_000, 20, 40,
        List("Airbus A320","Airbus A330-300"))
      case "Kenya Airways" => AirlineStrategyProfile(AirlineType.LEGACY, AIType.PASSIVE, AirlineFocusStrategy.FLAG_CARRIER, AIAlliance.PLACEHOLDER,
        "NBO", List("MBA","KIS"),
        List("ET","NG","EG","CD","TZ","ZA","KE","UG","DZ","GH"), List(), List("IN","GB"), List("Arabic","EU","ASEAN"),
        List("ET","NG","EG","CD","TZ","ZA","KE","UG","DZ","GH"), List(),
        0.6, 0.4, 200, 12_000, 20, 40,
        List("Airbus A320","Airbus A330-300"))
    }
    airlineProfile
  }

	case class AirlineStrategyProfile(
    airlineType: AirlineType.Value,
    aiType: AIType.Value,
    focusStrategy: AirlineFocusStrategy.Value,
    alliance: AIAlliance.Value,
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
    val FLAG_CARRIER, LOW_COST_DOMESTIC_CARRIER, REGIONAL_CARRIER, GLOBAL_PRESTIGE_CARRIER, LEGACY_CARRIER = Value
    val label: AirlineFocusStrategy => String = {
      case FLAG_CARRIER => "Flag Carrier"
      case LOW_COST_DOMESTIC_CARRIER => "Low-Cost Domestic Carrier"
      case REGIONAL_CARRIER => "Regional Carrier"
      case GLOBAL_PRESTIGE_CARRIER => "Global Prestige Carrier"
      case LEGACY_CARRIER => "Legacy Carrier"
    }
    def fromId(id: Int): AirlineFocusStrategy = id match {
      case 0 => FLAG_CARRIER
      case 1 => LOW_COST_DOMESTIC_CARRIER
      case 2 => REGIONAL_CARRIER
      case 3 => GLOBAL_PRESTIGE_CARRIER
      case 4 => LEGACY_CARRIER
      case _ => throw new IllegalArgumentException("Invalid AirlineFocusStrategy ID: " + id)
    }
  }
}
