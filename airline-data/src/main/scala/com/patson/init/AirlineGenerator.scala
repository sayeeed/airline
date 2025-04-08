package com.patson.init

import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer
import com.patson.util._
import com.patson.data._
import com.patson.data.Constants._
import com.patson.model._
import com.patson.model.airplane._

import java.util.Calendar
import com.patson.Authentication

import scala.util.Random
import com.patson.DemandGenerator
import com.patson.UserSimulation.configFactory
import com.patson.data._
import com.patson.data.airplane._
import com.typesafe.config.ConfigFactory

import java.util.concurrent.ThreadLocalRandom
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration


object AirlineGenerator extends App {
  mainFlow

  private lazy val allModels = ModelSource.loadAllModels()
  private lazy val airports = AirportSource.loadAllAirports(fullLoad = false, loadFeatures = true)
  private lazy val countryRelationships = CountrySource.getCountryMutualRelationships()
  private lazy val bufferOfAirplanes = mutable.Map[Model, (Airplane, Int)]()

  case class LinkConfig(description: String, poolSize: Int, linkCount: Int, rawQuality: Int)
  case class LinkGeneration(fromAirport: Airport, toAirports: List[Airport], models: List[Model], airline: Airline, config: LinkConfig)

  def mainFlow() = {
    deleteAirlines()
    
    // North America
    generateDeltaAirLines(List("US"))
    generateAmericanAirlines(List("US"))
    generateUnitedAirlines(List("US"))
    generateSouthwestAirlines(List("US"))

    // European
    generateRyanair(List("EU"))
    generateLufthansa(List("EU"))
    generateBritishAirways(List("EU"))
    generateAirFrance(List("EU"))
    generateKLM(List("EU"))
    generateEasyJet(List("EU"))
    generateTurkishAirlines(List("EU"))
    generateAeroflot(List("RU"))

    // South America
    generateLatamAirline()
    generateAvianca()

    // Asia
    generateEmirates()
    generateQatarAirways()
    generateSaudia()
    generateChinaSouthern(List("CN"))
    generateAirChina(List("CN"))
    generateChinaEastern(List("CN"))
    generateAllNipponAirways(List("JP"))
    generateJapanAirlines(List("JP"))
    generateCebuPacific(List("PH"))
    generateMalaysiaAirlines(List("MY"))
    generateGarudaIndonesia(List("ID"))
    generateQantas(List("AU"))

    // Africa
    generateSouthAfricanAirways(List("SADC"))
    generateAirPeace(List("ECOWAS"))
    generateEthiopianAirlines(List("EAC"))

    generateUSAirline(List("US"))
    generateCountryAirlines(List("CN","RU","IN","ID","BR"))
    generateSmallCountryAirlines(List("JP","CA","TR","MX","VN"))
    generateRemoteAirlines(List("AU","CA","US","DK","RU"))
    generateAffinityAirlines(List("EU","Banking","Oil","Pharma","Electronics","Copper","Marine"))
    generateAerospaceAirline()
    generateSSTAirline()
    generateLatamAirline()
    generateArabiaAirline()
    generateCaribbeanAirline()
    generatePacificAirline()
    
    resizeBases()

    println("DONE Creating airlines")
    Await.result(actorSystem.terminate(), Duration.Inf)
  }

  def deleteAirlines() : Unit = {
    println("Deleting airlines...")
    UserSource.deleteGeneratedUsers()
    UserCache.invalidateAll()
    AirlineCache.invalidateAll()
    AirlineSource.deleteAirlinesByCriteria(List(("airline_type", AirlineType.NON_PLAYER.id)))
    AirplaneOwnershipCache.invalidateAll()
  }  
  
  def generateDeltaAirLines(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "JFK" || airport.iata == "BOS" || airport.iata == "DTW" || airport.iata == "LAX" || airport.iata == "MSP" || airport.iata == "LGA" || airport.iata == "SLC" || airport.iata == "SEA")
      val HQ = airports.find(_.iata == "ATL").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Delta Air Lines",
        s"deltaairlines",
        HQ,
        bases,
        toAirports,
        List("Boeing 717-200", "Boeing 737-800", "Boeing 737-900ER", "Boeing 757-200", "Boeing 767-300", "Airbus A220-300", "Airbus A321", "Airbus A330-300", "Airbus A350-900"),
        8000,
        60
      )
    })
  }

  def generateAmericanAirlines(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "ORD" || airport.iata == "CLT" || airport.iata == "LAX" || airport.iata == "MIA" || airport.iata == "JFK" || airport.iata == "LGA" || airport.iata == "PHL" || airport.iata == "PHX" || airport.iata == "DCA")
      val HQ = airports.find(_.iata == "DFW").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"American Airlines",
        s"americanairlines",
        HQ,
        bases,
        toAirports,
        List("Airbus A319", "Airbus A321", "Boeing 737-800", "Boeing 737 MAX 8", "Boeing 777-200", "Boeing 787-8 Dreamliner"),
        8000,
        60
      )
    })
  }

  def generateUnitedAirlines(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "DEN" || airport.iata == "GUM" || airport.iata == "IAH" || airport.iata == "LAX" || airport.iata == "EWR" || airport.iata == "SFO" || airport.iata == "IAD")
      val HQ = airports.find(_.iata == "ORD").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"United Airlines",
        s"unitedairlines",
        HQ,
        bases,
        toAirports,
        List("Airbus A319", "Airbus A320", "Boeing 737-800", "Boeing 737-900ER", "Boeing 737 MAX 8", "Boeing 737 MAX 9", "Boeing 757-200", "Boeing 767-300", "Boeing 777-200", "Boeing 787-9 Dreamliner"),
        8000,
        60
      )
    })
  }

  def generateSouthwestAirlines(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "HOU" || airport.iata == "LAS" || airport.iata == "DEN" || airport.iata == "OAK" || airport.iata == "PHX" || airport.iata == "BWI" || airport.iata == "MDW" || airport.iata == "MCO" || airport.iata == "LAX" || airport.iata == "FLL" || airport.iata == "AUS")
      val HQ = airports.find(_.iata == "DAL").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Southwest Airlines",
        s"southwestairlines",
        HQ,
        bases,
        toAirports,
        List("Boeing 737-700", "Boeing 737-800", "Boeing 737 MAX 8"),
        8000,
        50
      )
    })
  }

  def generateRyanair(affinities : List[String]): Unit = {
    affinities.foreach(affinity => {
      val bases = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity)).takeRight(8).reverse
      val HQ = airports.find(_.iata == "STN").getOrElse(bases.head)
      val toAirports = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity))
      generateAirline(
        s"Ryanair",
        s"ryanair",
        bases.head,
        bases.tail,
        toAirports,
        List("Boeing 737-800", "Boeing 737 MAX 8"),
        5000,
        50
      )
    })
  }

  def generateLufthansa(affinities : List[String]): Unit = {
    affinities.foreach(affinity => {
      val bases = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity)).takeRight(11).reverse
      val HQ = airports.find(_.iata == "FRA").getOrElse(bases.head)
      val toAirports = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity))
      generateAirline(
        s"Lufthansa",
        s"lufthansa",
        bases.head,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321", "Airbus A340-300", "Airbus A350-900", "Airbus A380-800", "Boeing 747-8", "Bombardier CRJ900"),
        15000,
        60
      )
    })
  }

  def generateBritishAirways(affinities : List[String]): Unit = {
    affinities.foreach(affinity => {
      val bases = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity)).takeRight(11).reverse
      val HQ = airports.find(_.iata == "LHR").getOrElse(bases.head)
      val toAirports = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity))
      generateAirline(
        s"British Airways",
        s"britishairways",
        bases.head,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A350-1000", "Airbus A380-800", "Boeing 777-200", "Boeing 787-9 Dreamliner", "Embrear E190"),
        15000,
        60
      )
    })
  }

  def generateAirFrance(affinities : List[String]): Unit = {
    affinities.foreach(affinity => {
      val bases = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity)).takeRight(11).reverse
      val HQ = airports.find(_.iata == "CDG").getOrElse(bases.head)
      val toAirports = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity))
      generateAirline(
        s"Air France",
        s"airfrance",
        bases.head,
        bases.tail,
        toAirports,
        List("Airbus A220-300", "Airbus A320", "Airbus A350-900", "Airbus A330-200", "Boeing 777-300ER", "Boeing 787-9 Dreamliner"),
        15000,
        60
      )
    })
  }

  def generateKLM(affinities : List[String]): Unit = {
    affinities.foreach(affinity => {
      val bases = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity)).takeRight(11).reverse
      val HQ = airports.find(_.iata == "AMS").getOrElse(bases.head)
      val toAirports = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity))
      generateAirline(
        s"KLM",
        s"klm",
        bases.head,
        bases.tail,
        toAirports,
        List("Boeing 737-800", "Boeing 777-300ER", "Boeing 787-9 Dreamliner", "Boeing 787-10 Dreamliner"),
        15000,
        60
      )
    })
  }

  def generateEasyJet(affinities : List[String]): Unit = {
    affinities.foreach(affinity => {
      val bases = airports.filter(airport => airport.iata == "BFS" || airport.iata == "BHX" || airport.iata == "BRS" || airport.iata == "EDI" || airport.iata == "GLA" || airport.iata == "LPL" || airport.iata == "SEN" || airport.iata == "LGW" || airport.iata == "MAN")
      val HQ = airports.find(_.iata == "LTN").getOrElse(bases.head)
      val toAirports = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity))
      generateAirline(
        s"easyJet",
        s"easyjet",
        bases.head,
        bases.tail,
        toAirports,
        List("Airbus A319", "Airbus A320", "Airbus A320neo", "Airbus A321neo"),
        5000,
        50
      )
    })
  }

  def generateTurkishAirlines(affinities : List[String]): Unit = {
    affinities.foreach(affinity => {
      val bases = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity)).takeRight(8).reverse
      val HQ = airports.find(_.iata == "IST").getOrElse(bases.head)
      val toAirports = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity))
      generateAirline(
        s"Turkish Airlines",
        s"turkishairlines",
        bases.head,
        bases.tail,
        toAirports,
        List("Airbus A321", "Airbus A321neo", "Airbus A350-900", "Boeing 373-800", "Boeing 777-300ER", "Boeing 787-9 Dreamliner"),
        15000,
        60
      )
    })
  }

  def generateAeroflot(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filterNot(_.isDomesticAirport()).filter(_.countryCode == countryCode).takeRight(8).reverse
      val HQ = airports.find(_.iata == "SVO").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Aeroflot",
        s"aeroflot",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Boeing 737-800", "Boeing 777-300ER"),
        15000,
        45
      )
    })
  }

  def generateLatamAirline(): Unit = {
    val bases = airports.filter(airport => airport.zone.contains("Hispanic") && airport.isGateway() && airport.countryCode != "US").takeRight(8).reverse
    val HQ = airports.find(_.iata == "GRU").getOrElse(bases.head)
    val toAirports = airports.filter(port => port.zone.contains("Hispanic") || port.zone.contains("Lusophone")).distinct
    generateAirline(
      s"LATAM Airlines",
      s"latamairlines",
      HQ,
      bases.tail,
      toAirports,
      List("Airbus A320", "Airbus A321", "Boeing 767-300", "Boeing 787-9 Dreamliner"),
      8000,
      55
    )
  }

  def generateAvianca(): Unit = {
    val bases = airports.filter(airport => airport.iata == "LIM" || airport.iata == "SCL" || airport.iata == "PTY" || airport.iata == "CCS" || airport.iata == "UIO" || airport.iata == "LPB")
    val HQ = airports.find(_.iata == "BOG").getOrElse(bases.head)
    val toAirports = airports.filter(port => port.zone.contains("Hispanic") || port.zone.contains("Lusophone")).distinct
    generateAirline(
      s"Avianca",
      s"avianca",
      bases.head,
      bases.tail,
      toAirports,
      List("Airbus A320", "Airbus A321", "Boeing 767-300", "Boeing 787-9 Dreamliner"),
      8000,
      50
    )
  }

  def generateEmirates(): Unit = {
    val bases = airports.filter(airport => airport.iata == "DOH" || airport.iata == "CAI" || airport.iata == "AUH" || airport.iata == "IST" || airport.iata == "BGW" || airport.iata == "IKA")
    val HQ = airports.find(_.iata == "DXB").getOrElse(bases.head)
    val destinations = List("IST", "FRA", "BER", "IKA", "SVO", "LHR", "SIN")
    val toAirports = airports.filter(port => port.zone.contains("Sunni") || destinations.contains(port.iata)).distinct
    generateAirline(
      s"Emirates",
      s"emirates",
      HQ,
      bases.tail,
      toAirports,
      List("Airbus A319", "Airbus A380-800", "Boeing 777-300ER"),
      15000,
      70
    )
  }

  def generateQatarAirways(): Unit = {
    val bases = airports.filter(airport => airport.iata == "DXB" || airport.iata == "AUH" || airport.iata == "IST" || airport.iata == "BAH" || airport.iata == "MCT" || airport.iata == "KWI")
    val HQ = airports.find(_.iata == "DOH").getOrElse(bases.head)
    val destinations = List("IST", "FRA", "BER", "IKA", "SVO", "LHR", "SIN")
    val toAirports = airports.filter(port => port.zone.contains("Sunni") || destinations.contains(port.iata)).distinct
    generateAirline(
      s"Qatar Airways",
      s"qatarairways",
      HQ,
      bases.tail,
      toAirports,
      List("Airbus A319", "Airbus A380-800", "Boeing 777-300ER"),
      15000,
      70
    )
  }

  def generateSaudia(): Unit = {
    val bases = airports.filter(airport => airport.iata == "RUH" || airport.iata == "AUH" || airport.iata == "TLV" || airport.iata == "IST" || airport.iata == "CAI" || airport.iata == "DOH")
    val HQ = airports.find(_.iata == "JED").getOrElse(bases.head)
    val destinations = List("IST", "FRA", "BER", "IKA", "SVO", "LHR", "SIN")
    val toAirports = airports.filter(port => port.zone.contains("Sunni") || destinations.contains(port.iata)).distinct
    generateAirline(
      s"Saudia",
      s"saudia",
      HQ,
      bases.tail,
      toAirports,
      List("Airbus A319", "Airbus A380-800", "Boeing 777-300ER"),
      15000,
      70
    )
  }

  def generateChinaSouthern(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "PEK" || airport.iata == "PVG" || airport.iata == "SZX" || airport.iata == "CKG" || airport.iata == "URC" || airport.iata == "WUH" || airport.iata == "CGO")
      val HQ = airports.find(_.iata == "CAN").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"China Southern",
        s"chinasouthern",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Airbus A330-300", "Boeing 737-800", "Comac C909 ER"),
        10000,
        50
      )
    })
  }

  def generateAirChina(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "PKX" || airport.iata == "PVG" || airport.iata == "CTU" || airport.iata == "CFU")
      val HQ = airports.find(_.iata == "PEK").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Air China",
        s"airchina",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Airbus A330-300", "Boeing 737-800", "Comac C909 ER"),
        10000,
        50
      )
    })
  }

  def generateChinaEastern(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "SHA" || airport.iata == "KMG" || airport.iata == "XIY" || airport.iata == "PKX" || airport.iata == "TFU" || airport.iata == "HGH" || airport.iata == "NKG" || airport.iata == "TAO" || airport.iata == "XMN")
      val HQ = airports.find(_.iata == "PVG").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"China Eastern",
        s"chinaeastern",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Airbus A330-300", "Boeing 737-800", "Comac C909 ER"),
        10000,
        50
      )
    })
  }

  def generateIndigo(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "BOM" || airport.iata == "HYD" || airport.iata == "BLR" || airport.iata == "COK" || airport.iata == "CCU" || airport.iata == "MAA")
      val HQ = airports.find(_.iata == "DEL").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"IndiGo",
        s"indigo",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Boeing 777-300ER"),
        5000,
        45
      )
    })
  }

  def generateAirIndia(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "BOM" || airport.iata == "BLR")
      val HQ = airports.find(_.iata == "DEL").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Air India",
        s"airindia",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Boeing 777-300ER", "Boeing 787-9 Dreamliner"),
        10000,
        55
      )
    })
  }

  def generateAllNipponAirways(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "NRT" || airport.iata == "KIX" || airport.iata == "OTM")
      val HQ = airports.find(_.iata == "HND").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"All Nippon Airways",
        s"allnipponairways",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Boeing 737-800", "Boeing 787-9 Dreamliner"),
        15000,
        60
      )
    })
  }

  def generateJapanAirlines(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "NRT" || airport.iata == "KIX" || airport.iata == "OTM")
      val HQ = airports.find(_.iata == "HND").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Japan Airlines",
        s"japanairlines",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Boeing 737-800", "Boeing 787-9 Dreamliner"),
        15000,
        60
      )
    })
  }

  def generateCebuPacific(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "CEB" || airport.iata == "CRK" || airport.iata == "DVO" || airport.iata == "ILO")
      val HQ = airports.find(_.iata == "MNL").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Cebu Pacific",
        s"cebupacific",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Boeing 737-800", "Boeing 787-9 Dreamliner"),
        10000,
        60
      )
    })
  }

  def generateMalaysiaAirlines(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "BKI" || airport.iata == "KCH")
      val HQ = airports.find(_.iata == "KUL").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Malaysia Airlines",
        s"malaysiaairlines",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Boeing 737-800", "Boeing 787-9 Dreamliner"),
        10000,
        60
      )
    })
  }
  
  def generateGarudaIndonesia(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "DPS" || airport.iata == "SUB" || airport.iata == "UPG")
      val HQ = airports.find(_.iata == "CGK").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Garuda Indonesia",
        s"garudaindonesia",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Boeing 737-800", "Boeing 787-9 Dreamliner"),
        10000,
        60
      )
    })
  }

  def generateQantas(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(airport => airport.iata == "BNE" || airport.iata == "MEL" || airport.iata == "PER")
      val HQ = airports.find(_.iata == "SYD").getOrElse(bases.head)
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Qantas",
        s"qantas",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Boeing 737-800", "Boeing 787-9 Dreamliner"),
        15000,
        60
      )
    })
  }

  def generateSouthAfricanAirways(affinities : List[String]): Unit = {
    affinities.foreach(affinity => {
      val bases = airports.filter(airport => airport.iata == "CPT")
      val HQ = airports.find(_.iata == "JNB").getOrElse(bases.head)
      val toAirports = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity))
      generateAirline(
        s"South African Airways",
        s"southafricanairways",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Boeing 737-800", "Boeing 787-9 Dreamliner"),
        10000,
        60
      )
    })
  }

  def generateAirPeace(affinities : List[String]): Unit = {
    affinities.foreach(affinity => {
      val bases = airports.filter(airport => airport.iata == "ABV")
      val HQ = airports.find(_.iata == "LOS").getOrElse(bases.head)
      val toAirports = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity))
      generateAirline(
        s"Air Peace",
        s"airpeace",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Boeing 737-800", "Boeing 787-9 Dreamliner"),
        10000,
        50
      )
    })
  }

  def generateEthiopianAirlines(affinities : List[String]): Unit = {
    affinities.foreach(affinity => {
      val bases = airports.filter(airport => airport.iata == "LLW" || airport.iata == "LFW" || airport.iata == "LUN")
      val HQ = airports.find(_.iata == "ADD").getOrElse(bases.head)
      val toAirports = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity))
      generateAirline(
        s"Ethiopian Airlines",
        s"ethiopianairlines",
        HQ,
        bases.tail,
        toAirports,
        List("Airbus A320", "Airbus A321neo", "Airbus A350-900", "Boeing 737-800", "Boeing 787-9 Dreamliner"),
        10000,
        50
      )
    })
  }

  def generateUSAirline(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filterNot(_.isDomesticAirport()).filter(_.countryCode == countryCode).takeRight(11).reverse
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Rats ${countryCode}",
        s"R${countryCode}",
        bases.head,
        bases.tail,
        toAirports,
        List("Boeing 737-800", "Boeing 737-700", "Boeing 737-600", "Boeing 777-300ER"),
        8000,
        60
      )
    })
  }

  def generateCountryAirlines(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filterNot(_.isDomesticAirport()).filter(_.countryCode == countryCode).takeRight(8).reverse
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Rats ${countryCode}",
        s"R${countryCode}",
        bases.head,
        bases.tail,
        toAirports,
        List("Embraer E195-E2", "Embraer E175-E2", "Comac C929-600"),
        8000,
        60
      )
    })
  }

  def generateSmallCountryAirlines(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filterNot(_.isDomesticAirport()).filter(_.countryCode == countryCode).takeRight(6).reverse
      val toAirports = airports.filter(_.countryCode == countryCode)
      generateAirline(
        s"Rats ${countryCode}",
        s"R${countryCode}",
        bases.head,
        bases.tail,
        toAirports,
        List("McDonnell Douglas DC-8-61", "McDonnell Douglas DC-8-62", "McDonnell Douglas DC-8-63", "Boeing 727-100", "Boeing 727-200"),
        8000,
        40
      )
    })
  }

  def generateAffinityAirlines(affinities : List[String]): Unit = {
    affinities.foreach(affinity => {
      val bases = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity)).takeRight(11).reverse
      val toAirports = airports.filterNot(_.isDomesticAirport()).filter(_.zone.contains(affinity))
      generateAirline(
        s"${affinity} Rats",
        s"${affinity}",
        bases.head,
        bases.tail,
        toAirports,
        List("Embraer E190", "Embraer E170", "Boeing 787-8 Dreamliner"),
        10000,
        80
      )
    })
  }

  def generateAerospaceAirline(): Unit = {
    val bases = airports.filter(_.zone.contains("Aerospace")).takeRight(11)
    val toAirports = airports.filter(port => port.zone.contains("Aerospace") || port.zone.contains("Space") || port.zone.contains("Software") || port.size >= 7).distinct
    val HQ = airports.find(_.iata == "TLS").getOrElse(bases.head)
    println(HQ)
    generateAirline(
      s"Aerospace Rats",
      s"Aerospace",
      HQ,
      bases,
      toAirports,
      List("Boeing 797-6", "Boeing 797-7", "Boeing 787-8 Dreamliner", "Boeing 787-9 Dreamliner"),
      12000,
      85
    )
  }

  def generateSSTAirline(): Unit = {
    val bases = airports.filter(airport => airport.iata == "ORD" || airport.iata == "SFO" || airport.iata == "BOS" || airport.iata == "FAI" || airport.iata == "ANC" || airport.iata == "BET")
    val HQ = airports.find(_.iata == "JFK").getOrElse(bases.head)
    generateAirline(
      s"Y",
      s"y",
      HQ,
      bases,
      airports,
      List("Boeing 2707", "Beechcraft B200 Super King Air", "Boeing 737-800"),
      6000,
      85
    )
  }

  def generatePacificAirline(): Unit = {
    val bases = airports.filter(airport => airport.zone.contains("PIF") && airport.isGateway() && airport.countryCode != "AU" && airport.countryCode != "NZ" && airport.countryCode != "US").takeRight(7).reverse
    generateAirline(
      s"Echo Pacific",
      s"echopacific",
      bases.head,
      bases.tail,
      airports,
      List("Douglas DC-3", "Lockheed L-749 Constellation"),
      5000,
      45
    )
  }

  def generateArabiaAirline(): Unit = {
    val bases = airports.filter(airport => airport.zone.contains("Arabic")).takeRight(7).reverse
    val destinations = List("IST", "FRA", "BER", "IKA", "SVO", "LHR", "SIN")
    val toAirports = airports.filter(port => port.zone.contains("Sunni") || destinations.contains(port.iata)).distinct
    generateAirline(
      s"ONE Arabia",
      s"onearabia",
      bases.head,
      bases.tail,
      toAirports,
      List("Airbus A318", "Airbus A320", "Airbus A321neo", "Airbus A321neoXLR"),
      7000,
      70
    )
  }

  def generateCaribbeanAirline(): Unit = {
    val bases = airports.filter(airport => airport.zone.contains("CC") && airport.isGateway() && airport.countryCode != "US").takeRight(6).reverse
    val HQ = airports.find(_.iata == "MIA").getOrElse(bases.head)
    val toAirports = airports.filter(port => port.zone.contains("CC")).distinct
    generateAirline(
      s"Echo Caribbean",
      s"echocaribbean",
      HQ,
      bases,
      toAirports,
      List("Fokker 70", "Fokker 60", "Fokker 100"),
      6000,
      85
    )
  }

  def generateRemoteAirlines(countryCodes : List[String]): Unit = {
    countryCodes.foreach(countryCode => {
      val bases = airports.filter(_.countryCode == countryCode).filter(_.popMiddleIncome <= 9000).takeRight(7)
      val toAirports = airports.filter(_.countryCode == countryCode).filter(_.popMiddleIncome <= 50000)
      generateAirline(
        s"Rats ${countryCode}",
        s"rr${countryCode}",
        bases.head,
        bases.tail,
        toAirports,
        List("Cessna 208 Caravan", "Cessna 408 Skycourier"),
        1800,
        50
      )
    })
  }

  def generateAirline(name: String, username: String, hqAirport: Airport, bases: List[Airport], toAirports: List[Airport], modelFamily: List[String], linkMaxDistance: Int, targetServiceQuality: Int, initialBalance: Long = 1000000000, currentServiceQuality: Int = 70, reputation: Int = 80): Airline = {
    val user = createUser(username)
    val airline = createAirline(name, hqAirport, targetServiceQuality, currentServiceQuality, reputation, initialBalance)
    println(s"generating $name at ${hqAirport.iata} with ${modelFamily.toString()}")

    AirlineSource.saveAirlines(List(airline))
    UserSource.setUserAirline(user, airline)
    AirlineSource.saveAirlineInfo(airline, false)
    AirlineSource.saveAirplaneRenewal(airline.id, 60)

    // Generate bases & links
    makeBase(airline, hqAirport, true)
    generateLinksForAirline(airline, hqAirport, toAirports, modelFamily, linkMaxDistance + 1000)
    bases.zipWithIndex.foreach { case (baseAirport, index) => makeBase(airline, baseAirport) }
    bases.foreach( airport => generateLinksForAirline(airline, airport, toAirports, modelFamily, linkMaxDistance))
    airline
  }

  private def makeBase(airline: Airline, airport: Airport, isHq: Boolean = false, scale: Int = 7, foundedCycle: Int = 1) = {
    val base = AirlineBase(airline, airport, airport.countryCode, scale, foundedCycle, isHq)
    AirlineSource.saveAirlineBase(base)
  }

  private def createUser(userName: String): User = {
    val user = User(userName = userName, email = "bot", Calendar.getInstance, Calendar.getInstance, UserStatus.ACTIVE, level = 0, None, List.empty)

    val devMode = if (configFactory.hasPath("dev")) configFactory.getBoolean("dev") else false
    val password = if (devMode) "12345" else Random.nextInt(5000).toString
    UserSource.saveUser(user)
    Authentication.createUserSecret(userName, password)

    user
  }

  private def createAirline(name: String, hqAirport: Airport, targetServiceQuality: Int, currentServiceQuality: Int, reputation: Int, initialBalance: Long): Airline = {
    val airline = Airline(name + " [bot]", AirlineType.NON_PLAYER)
    airline.setBalance(initialBalance)
    airline.setTargetServiceQuality(targetServiceQuality)
    airline.setCurrentServiceQuality(currentServiceQuality)
    airline.setReputation(reputation)
    airline.setSkipTutorial(true)
    airline.setCountryCode(hqAirport.countryCode)
    airline.setAirlineCode(airline.getDefaultAirlineCode())
    airline
  }

  private def generateLinksForAirline(airline: Airline, baseAirport: Airport, toAirports: List[Airport], modelNames: List[String], maxDistance: Int): Unit = {
    val models = allModels.filter(model => modelNames.contains(model.name))
    val nearbyFocusAirports = findAirports(toAirports, baseAirport, maxDistance / 4)
    val farAirports = findAirports(airports, baseAirport, maxDistance, maxDistance / 4).filterNot(airport => nearbyFocusAirports.contains(airport))
    bufferOfAirplanes.clear()

    generateLinks(
      LinkGeneration(
        fromAirport = baseAirport,
        toAirports = nearbyFocusAirports,
        models = models,
        airline = airline,
        config = LinkConfig("near", nearbyFocusAirports.size, Math.min(24, nearbyFocusAirports.size), 40)
      )
    )

    generateLinks(
      LinkGeneration(
        fromAirport = baseAirport,
        toAirports = farAirports,
        models = models,
        airline = airline,
        config = LinkConfig("far", farAirports.size, 8, 60)
      )
    )
  }

  private def findAirports(airports: List[Airport], baseAirport: Airport, maxDistance: Int, minDistance: Int = 300): List[Airport] = {
    val relationships = CountrySource.getCountryMutualRelationships()

    airports.filter(airport => {
      val distance = Computation.calculateDistance(baseAirport, airport)
      val relationship = relationships.getOrElse(
        (baseAirport.countryCode, airport.countryCode),
        0
      )

      airport.id != baseAirport.id &&
        relationship >= 0 &&
        distance > minDistance &&
        distance < maxDistance
    }).reverse
  }

  private def generateLinks(config: LinkGeneration): List[Link] = {
    val airports = config.toAirports.takeRight(config.config.poolSize + 1)
    val pickedToAirports = drawFromPool(airports, config.config.poolSize)

    val airplaneModelsLarge = config.models.sortBy(_.capacity).reverse
    val airplaneModelsSmall = config.models.sortBy(_.capacity)
    val newLinks = ListBuffer[Link]()

    var i = 0
    while (newLinks.length < config.config.linkCount && i < config.config.poolSize) {
      val toAirport = pickedToAirports(i)
      i += 1

      val distance = Computation.calculateDistance(config.fromAirport, toAirport)
      val relationship = countryRelationships.getOrElse((config.fromAirport.countryCode, toAirport.countryCode), 0)

      val affinity = Computation.calculateAffinityValue(config.fromAirport.zone, toAirport.zone, relationship)

      val demand = DemandGenerator.computeBaseDemandBetweenAirports(config.fromAirport, toAirport, affinity, distance)
      val targetSeats = (demand.travelerDemand.total + demand.businessDemand.total) * 2

      if (targetSeats > 0) {
        createLink(
          fromAirport = config.fromAirport,
          toAirport = toAirport,
          airline = config.airline,
          distance = distance,
          targetSeats = targetSeats,
          modelsSmall = airplaneModelsSmall,
          modelsLarge = airplaneModelsLarge,
          rawQuality = config.config.rawQuality
        ).foreach(newLinks += _)
      }
    }

    if (newLinks.nonEmpty) {
      LinkSource.saveLinks(newLinks.filter(_.frequency > 0).toList)
    } else {
      println(
        s"No links on ${config.config.description} from ${config.fromAirport.iata} !!!"
      )
    }

    newLinks.toList
  }

  private def createLink(fromAirport: Airport, toAirport: Airport, airline: Airline, distance: Int, targetSeats: Int, modelsSmall: List[Model], modelsLarge: List[Model], rawQuality: Int): Option[Link] = {
    val pickedModel = modelsSmall.find(model =>
        model.capacity * Computation.calculateMaxFrequency(
          model,
          distance
        ) >= targetSeats && model.range >= distance && toAirport.runwayLength >= model.runwayRequirement
      ).orElse(modelsLarge.find(model => model.range >= distance && model.runwayRequirement <= toAirport.runwayLength))

    pickedModel.flatMap { model =>
      val frequency = math.min(
        (targetSeats.toDouble / model.capacity).toInt,
        35
      )

      if (frequency > 0) {
        val maxFrequencyPerAirplane = Computation.calculateMaxFrequency(model, distance)
        val airplanesRequired = math.max(1, frequency / maxFrequencyPerAirplane)

        val assignedAirplanes = createAndAssignAirplanes(
          model = model,
          airline = airline,
          homeAirport = fromAirport,
          frequency = frequency,
          distance = distance,
          airplanesRequired = airplanesRequired,
          maxFrequencyPerAirplane = maxFrequencyPerAirplane
        )

        val priceMod = if (fromAirport.popMiddleIncome < 100_000 || toAirport.popMiddleIncome < 100_000)
          0.8
        else if (fromAirport.popMiddleIncome > 1_000_000 || toAirport.popMiddleIncome > 1_000_000)
          1.1
        else
          1.0

        val econPrice = (priceMod * Pricing.computeStandardPrice(distance, Computation.getFlightCategory(fromAirport, toAirport), ECONOMY, PassengerType.BUSINESS, fromAirport.baseIncome)).toInt
        val bizPrice = (priceMod * Pricing.computeStandardPrice(distance, Computation.getFlightCategory(fromAirport, toAirport), BUSINESS, PassengerType.BUSINESS, fromAirport.baseIncome)).toInt
        val firstPrice = (priceMod * Pricing.computeStandardPrice(distance, Computation.getFlightCategory(fromAirport, toAirport), FIRST, PassengerType.BUSINESS, fromAirport.baseIncome)).toInt

        val duration = Computation.calculateDuration(model, distance)
        val capacity = calculateTotalCapacity(assignedAirplanes)

        val link = Link(
          fromAirport,
          toAirport,
          airline,
          LinkClassValues(econPrice, bizPrice, firstPrice),
          distance,
          capacity,
          rawQuality,
          duration = duration,
          frequency = frequency
        )

        link.setAssignedAirplanes(assignedAirplanes)
        Some(link)
      } else {
//        println(s"Cannot generate link from ${fromAirport.iata} to ${toAirport.iata} frequency is 0")
        None
      }
    }
  }

  private def createAndAssignAirplanes(model: Model, airline: Airline, homeAirport: Airport, frequency: Int, distance: Int, airplanesRequired: Int, maxFrequencyPerAirplane: Int): Map[Airplane, LinkAssignment] = {
    val assignedAirplanes = mutable.Map[Airplane, LinkAssignment]()
    val flightMinutesRequired = Computation.calculateFlightMinutesRequired(model, distance)
    var remainingFrequency = frequency

    for (_ <- 0 until airplanesRequired if remainingFrequency > 0) {
      val matchingPlane = bufferOfAirplanes.getOrElse(model, null)

      val frequencyForThis = if (matchingPlane != null && matchingPlane._2 >= flightMinutesRequired) {
        val usedAirplane = matchingPlane._1
        val frequencyForThis = math.min(remainingFrequency, Math.floor(matchingPlane._2 / flightMinutesRequired)).toInt
        val flightMinutesForThis = frequencyForThis * flightMinutesRequired
        val remainingFlightMinutes = matchingPlane._2 - flightMinutesForThis
        if (remainingFlightMinutes > 120) {
          bufferOfAirplanes(model) = (usedAirplane, remainingFlightMinutes)
        } else {
          bufferOfAirplanes.remove(model)
        }
        assignedAirplanes.put(usedAirplane, LinkAssignment(frequencyForThis, flightMinutesForThis))
        frequencyForThis
      } else {
        val newAirplane = createAirplane(model, airline, homeAirport)
        val frequencyForThis = math.min(remainingFrequency, maxFrequencyPerAirplane)
        val flightMinutesForThis = frequencyForThis * flightMinutesRequired
        if (Airplane.MAX_FLIGHT_MINUTES - flightMinutesForThis > 120) {
          bufferOfAirplanes(model) = (newAirplane, Airplane.MAX_FLIGHT_MINUTES - flightMinutesForThis)
        }
        assignedAirplanes.put(newAirplane, LinkAssignment(frequencyForThis, flightMinutesForThis))
        frequencyForThis
      }


      remainingFrequency -= frequencyForThis
    }

    assignedAirplanes.toMap
  }

  private def createAirplane(model: Model, airline: Airline, homeAirport: Airport): Airplane = {
    val airplane = Airplane(
      model = model,
      owner = airline,
      constructedCycle = 0,
      purchasedCycle = 0,
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

  private def calculateTotalCapacity(assignedAirplanes: Map[Airplane, LinkAssignment]): LinkClassValues = {
    assignedAirplanes.map { case (airplane, assignment) =>
      LinkClassValues(airplane.configuration.economyVal, airplane.configuration.businessVal, airplane.configuration.firstVal) * assignment.frequency
    }.reduce(_ + _)
  }

  private def drawFromPool(poolTopFirst: Seq[Airport], drawSize: Int): Seq[Airport] = {
    if (drawSize >= poolTopFirst.length) {
      poolTopFirst
    } else {
      Random.shuffle(poolTopFirst).take(drawSize)
    }
  }

  private def resizeBases(): Unit = {
    val allAirlines = AirlineSource.loadAllAirlines(true)
    val allLinks = LinkSource.loadAllLinks(LinkSource.SIMPLE_LOAD)
    val allFlightLinksByAirlineId = allLinks.filter(_.transportType == TransportType.FLIGHT).map(_.asInstanceOf[Link]).groupBy(_.airline.id)
    allAirlines.foreach(airline => {
      val linksByFromAirportId = allFlightLinksByAirlineId.get(airline.id).getOrElse(List.empty).groupBy(_.from.id)
      airline.bases.foreach { base =>
        val staffRequired = linksByFromAirportId.get(base.airport.id) match {
          case Some(links) => links.map(_.getCurrentOfficeStaffRequired).sum
          case None => 0
        }
        val idealBaseLevel: Int = if (base.headquarter) Math.round(staffRequired.toDouble / 80).toInt else Math.round(staffRequired.toDouble / 60).toInt
        val updateBase = base.copy(scale = Math.max(1, idealBaseLevel))
        AirlineSource.saveAirlineBase(updateBase)
      }
    })
  }

}
