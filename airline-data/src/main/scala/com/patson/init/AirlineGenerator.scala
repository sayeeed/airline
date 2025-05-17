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
import com.patson.AISimulation
import com.patson.ai.AIAirlines
import com.patson.ai.AISimUtil
import com.patson.model.AllianceRole.MEMBER


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

    //createAlliances()
    
    // Generate AI Airlines
    // USA
    generateAIAirline("Delta Air Lines", "delta")
    generateAIAirline("American Airlines", "american")
    generateAIAirline("United Airlines", "united")
    generateAIAirline("Southwest Airlines", "southwest")
    generateAIAirline("JetBlue Airways", "jetblue")
    generateAIAirline("Alaska Airlines", "alaska")
    generateAIAirline("Delta Connection", "deltaconnection")
    generateAIAirline("United Express", "unitedexpress")
    generateAIAirline("American Eagle", "americaneagle")
    // Canada
    generateAIAirline("Air Canada", "aircanada")
    generateAIAirline("WestJet", "westjet")
    generateAIAirline("Porter Airlines", "porter")
    // Mexico
    generateAIAirline("Aeromexico", "aeromexico")
    generateAIAirline("Volaris", "volaris")
    // Central America
    generateAIAirline("Copa Airlines", "copa")
    generateAIAirline("Avianca El Salvador", "aviancaelsalvador")
    // South America
    generateAIAirline("LATAM Airlines", "latam")
    generateAIAirline("Avianca", "avianca")
    generateAIAirline("Azul Linhas Aereas", "azul")
    generateAIAirline("Gol Linhas Aereas", "gol")
    generateAIAirline("Aerolineas Argentinas", "aerolineas")
    // Europe
    generateAIAirline("Ryanair", "ryanair")
    generateAIAirline("Lufthansa", "lufthansa")
    generateAIAirline("Air France", "airfrance")
    generateAIAirline("KLM", "klm")
    generateAIAirline("British Airways", "british")
    generateAIAirline("Turkish Airlines", "turkish")
    generateAIAirline("easyJet", "easyjet")
    // Middle East
    generateAIAirline("Emirates", "emirates")
    generateAIAirline("Qatar Airways", "qatar")
    generateAIAirline("Etihad Airways", "etihad")
    generateAIAirline("Saudia", "saudia")
    // India
    generateAIAirline("IndiGo", "indigo")
    generateAIAirline("Air India", "airindia")
    // China
    generateAIAirline("Air China", "airchina")
    generateAIAirline("China Southern Airlines", "chinasouthern")
    generateAIAirline("China Eastern Airlines", "chinaeastern")
    // Asia
    generateAIAirline("Cathay Pacific", "cathay")
    generateAIAirline("Singapore Airlines", "singapore")
    generateAIAirline("All Nippon Airways", "allnippon")
    generateAIAirline("Japan Airlines", "japan")
    generateAIAirline("Korean Air", "korean")
    // Southeast Asia
    generateAIAirline("Malaysia Airlines", "malaysia")
    generateAIAirline("Garuda Indonesia", "garuda")
    generateAIAirline("Phillipine Airlines", "phillipine")
    generateAIAirline("Vietnam Airlines", "vietnam")
    generateAIAirline("Thai Airways", "thai")
    // Australia
    generateAIAirline("Qantas", "qantas")
    // Africa
    generateAIAirline("Ethiopian Airlines", "ethiopian")
    generateAIAirline("South African Airways", "southafrican")
    generateAIAirline("Kenya Airways", "kenya")

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

  def generateAIAirline(name: String, username: String): Airline = {
    // airline profile data
    val profile = AIAirlines.getAirlineStrategyProfile(name)
    
    val bases = airports.filter(airport => profile.bases.contains(airport.iata))
    val hq = airports.find(_.iata == profile.hqAirport).get

    val primaryToAirports = airports.filter { airport => 
      val notSmallAirport = airport.size > 3
      val isPrimaryCountry = profile.primaryCountriesServed.contains(airport.countryCode)
      val isPrimaryAffinity = airport.zone.split("\\|").map(_.trim).exists(affinity => profile.primaryAffinitiesServed.contains(affinity))
      notSmallAirport && (isPrimaryCountry || isPrimaryAffinity)
    }.distinct
    val primaryAirportIDs = primaryToAirports.map(_.id).toSet
    val secondaryToAirports = airports.filter { airport => 
      val notSmallAirport = airport.size > 3
      val isSecondaryCountry = profile.secondaryCountriesServed.contains(airport.countryCode)
      val isSecondaryAffinity = airport.zone.split("\\|").map(_.trim).exists(affinity => profile.secondaryAffinitiesServed.contains(affinity))
      val isNotPrimaryAirport = !primaryAirportIDs.contains(airport.id)
      notSmallAirport && (isSecondaryCountry || isSecondaryAffinity) && isNotPrimaryAirport
    }.distinct

    val user = createUser(username)
    val airline = createAIAirline(name, hq, profile.airlineType, profile.aiType, profile.targetServiceQuality)
    
    println(s"generating $name at ${hq.iata} with ${profile.airlineType} profile")

    AirlineSource.saveAirlines(List(airline))
    UserSource.setUserAirline(user, airline)
    AirlineSource.saveAirlineInfo(airline, false)
    AirlineSource.saveAirplaneRenewal(airline.id, 50)

    var maxLinksPerBase = 24
    var maxLongLinksPerBase = 8

    if (profile.airlineType == AirlineType.MEGA_HQ) {
      maxLinksPerBase = 48
      maxLongLinksPerBase = 24
    }

    makeBase(airline, hq, true)
    generateLinksForAirline(airline, hq, primaryToAirports, secondaryToAirports, profile.modelNames, profile.linkMaxDistance + 1000, maxLinksPerBase, maxLongLinksPerBase, profile.routeServiceLevel)
    bases.zipWithIndex.foreach { case (baseAirport, index) => makeBase(airline, baseAirport) }
    bases.foreach( airport => generateLinksForAirline(airline, airport, primaryToAirports, secondaryToAirports, profile.modelNames, profile.linkMaxDistance + 1000, maxLinksPerBase, maxLongLinksPerBase, profile.routeServiceLevel))
    
    //addToAlliance(airline)

    airline
  }

  private def makeBase(airline: Airline, airport: Airport, isHq: Boolean = false, scale: Int = 7, foundedCycle: Int = 1) = {
    val base = AirlineBase(airline, airport, airport.countryCode, scale, foundedCycle, isHq)
    AirlineSource.saveAirlineBase(base)
  }

  private def createUser(userName: String): User = {
    val user = User(userName = userName, email = "bot", Calendar.getInstance, Calendar.getInstance, UserStatus.ACTIVE, level = 0, None, List.empty)

    val devMode = if (configFactory.hasPath("dev")) configFactory.getBoolean("dev") else false
    //val password = if (devMode) "12345" else Random.nextInt(5000).toString
    val password = userName
    UserSource.saveUser(user)
    Authentication.createUserSecret(userName, password)

    user
  }

  private def createAIAirline(name: String, hqAirport: Airport, airlineType: AirlineType.Value, aiType: AIType.Value, targetServiceQuality: Int): Airline = {
    val airline = Airline(name, airlineType, aiType)
    airline.setBalance(2_000_000_000)
    airline.setTargetServiceQuality(targetServiceQuality)
    airline.setCurrentServiceQuality(35)
    airline.setReputation(60)
    airline.setSkipTutorial(true)
    airline.setCountryCode(hqAirport.countryCode)
    airline.setAirlineCode(airline.getDefaultAirlineCode())
    airline
  }

  private def generateLinksForAirline(airline: Airline, baseAirport: Airport, primaryToAirports: List[Airport], secondaryToAirports: List[Airport], modelNames: List[String], maxDistance: Int, maxLinksPerBase: Int, maxLongLinksPerBase: Int, standardRouteQuality: Int): Unit = {
    println("Generating links...")
    val models = allModels.filter(model => modelNames.contains(model.name))
    
    val primaryAirportIDs = primaryToAirports.map(_.id).toSet
    val secondaryAirportIDs = secondaryToAirports.map(_.id).toSet
    val baseSpecificAirports = airports.filter { airport =>
      val notSmallAirport = airport.size > 3
      val relationship = CountrySource.getCountryMutualRelationship(baseAirport.countryCode, airport.countryCode)
      val affinity = Computation.calculateAffinityValue(baseAirport.zone, airport.zone, relationship)
      val isNotPrimaryAirport = !primaryAirportIDs.contains(airport.id)
      val isNotSecondaryAirport = !secondaryAirportIDs.contains(airport.id)
      notSmallAirport && affinity > 1 && isNotPrimaryAirport && isNotSecondaryAirport
    }.distinct.sortBy(_.population).reverse
    baseSpecificAirports.foreach { airport =>
      val relationship = CountrySource.getCountryMutualRelationship(baseAirport.countryCode, airport.countryCode)
      val affinity = Computation.calculateAffinityValue(baseAirport.zone, airport.zone, relationship)
      //println(f"Airport: ${airport.iata}%-6s ${airport.countryCode}%-6s ${airport.city}%-25s ${airport.population}%-10s ${affinity}%-5s")
    }

    //val modifiedPrimaryToAirports = (primaryToAirports ++ baseSpecificAirports).distinct
    val modifiedSecondaryToAirports = (secondaryToAirports ++ baseSpecificAirports).distinct
    //println(s"MODIFIED AIRPORTS SIZE: ${secondaryToAirports.size} and ${baseSpecificAirports.size}")

    bufferOfAirplanes.clear()

    generateLinks(
      LinkGeneration(
        fromAirport = baseAirport,
        toAirports = primaryToAirports,
        models = models,
        airline = airline,
        config = LinkConfig("near", primaryToAirports.size, Math.min(maxLinksPerBase, primaryToAirports.size), standardRouteQuality)
      )
    )

    generateLinks(
      LinkGeneration(
        fromAirport = baseAirport,
        toAirports = modifiedSecondaryToAirports,
        models = models,
        airline = airline,
        config = LinkConfig("far", modifiedSecondaryToAirports.size, maxLongLinksPerBase, standardRouteQuality)
      )
    )
  }

  private def findAirports(airports: List[Airport], baseAirport: Airport, maxDistance: Int, minDistance: Int = 200): List[Airport] = {
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
    val airlineProfile = AIAirlines.getAirlineStrategyProfile(config.airline.name)
    val airplaneModelsLarge = config.models.sortBy(_.capacity).reverse
    val airplaneModelsSmall = config.models.sortBy(_.capacity)
    
    val scoredRoutes = config.toAirports.filter(_.id != config.fromAirport.id).flatMap { toAirport =>
      val demand = AISimUtil.getLinkDemand(config.fromAirport, toAirport)
      val totalDemand = DemandGenerator.addUpDemands(demand)
      val distance = Computation.calculateDistance(config.fromAirport, toAirport)
      val targetSeats = ((demand.travelerDemand.total + demand.businessDemand.total) * 3).toInt

      if (targetSeats > 0 && totalDemand > airlineProfile.minimumRouteDemand) {
        val score = scoreNewLink(config.airline, airlineProfile, config.fromAirport, toAirport, totalDemand)
        Some(AISimulation.ScoredRoute(toAirport, score, distance, targetSeats))
      } else {
        None
      }
    }.sortBy(-_.score)

    //scoredRoutes.foreach { route =>
      //println(f"Scored routes: ${config.fromAirport.city}%-30s ${route.toAirport.city}%-30s ${route.score}%-5s")
    //}

    val topRoutes = scoredRoutes.sortBy(-_.score).take(config.config.linkCount)

    val newLinks = topRoutes.flatMap { link =>
      createLink(
        fromAirport = config.fromAirport,
        toAirport = link.toAirport,
        airline = config.airline,
        distance = link.distance,
        targetSeats = link.targetSeats,
        modelsSmall = airplaneModelsSmall,
        modelsLarge = airplaneModelsLarge,
        rawQuality = config.config.rawQuality
      )  
    }

    if (newLinks.nonEmpty) {
      LinkSource.saveLinks(newLinks.filter(_.frequency > 0).distinct.toList)
    } else {
      println(
        s"No links on ${config.config.description} from ${config.fromAirport.iata} !!!"
      )
    }

    newLinks
  }

  private def createLink(fromAirport: Airport, toAirport: Airport, airline: Airline, distance: Int, targetSeats: Int, modelsSmall: List[Model], modelsLarge: List[Model], rawQuality: Int): Option[Link] = {
    val pickedModel = modelsSmall.find(model =>
        model.capacity * Computation.calculateMaxFrequency(
          model,
          distance
        ) >= targetSeats && model.range >= distance && toAirport.runwayLength >= model.runwayRequirement
      ).orElse(modelsLarge.find(model => model.range >= distance && model.runwayRequirement <= toAirport.runwayLength))

    pickedModel.flatMap { model =>
      val rawFrequency = targetSeats.toDouble / model.capacity
      val closestMultipleOf7 = (Math.ceil(rawFrequency / 7.0) * 7).toInt
      val frequency = Math.min(closestMultipleOf7, 35)

      //println(s"Generating link ${fromAirport.iata}-${toAirport.iata} with ${frequency} frequency")

      if (frequency > 0) {
        val maxFrequencyPerAirplane = Computation.calculateMaxFrequency(model, distance)
        val airplanesRequired = math.max(1, Math.ceil(frequency.toDouble / maxFrequencyPerAirplane).toInt)

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
          1.0
        else
          1.0

        val econPrice = (priceMod * Pricing.computeStandardPrice(distance, Computation.getFlightCategory(fromAirport, toAirport), ECONOMY, PassengerType.TOURIST, fromAirport.baseIncome)).toInt
        val bizPrice = (priceMod * Pricing.computeStandardPrice(distance, Computation.getFlightCategory(fromAirport, toAirport), BUSINESS, PassengerType.TOURIST, fromAirport.baseIncome)).toInt
        val firstPrice = (priceMod * Pricing.computeStandardPrice(distance, Computation.getFlightCategory(fromAirport, toAirport), FIRST, PassengerType.TOURIST, fromAirport.baseIncome)).toInt

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

  private def scoreNewLink(airline: Airline, airlineProfile: AIAirlines.AirlineStrategyProfile, fromAirport: Airport, toAirport: Airport, totalDemand: Int) : Double = {
    val baseWeight = 100
    val populationScore = fromAirport.population / 100_000_000.0
    val incomeScore = fromAirport.income / 100_000.0
    val demandScore = totalDemand / 2_000.0
    val relationshipScore = 1.0

    baseWeight * populationScore * incomeScore * demandScore * relationshipScore
  }

  private def createAlliances() = {
    val skyTeam = Alliance("SkyTeam", 1, List(), AIAlliance.SKYTEAM.id)
    AllianceSource.saveAlliance(skyTeam)
  }

  private def addToAlliance(airline: Airline) = {
    val profile = AIAirlines.getAirlineStrategyProfile(airline.name)
    val allianceMember = AllianceMember(profile.alliance.id + 1, airline, role = MEMBER, 1)
    AllianceSource.saveAllianceMember(allianceMember)

  }

  object AIAlliance extends Enumeration {
    type AIAlliance = Value
    val SKYTEAM, ONEWORLD, STAR_ALLIANCE, PLACEHOLDER = Value
    val label: AIAlliance => String = {
      case SKYTEAM => "SkyTeam"
      case ONEWORLD => "Oneworld"
      case STAR_ALLIANCE => "Star Alliance"
      case PLACEHOLDER => "Placeholder"
    }
    def fromId(id: Int): AIAlliance = id match {
      case 0 => SKYTEAM
      case 1 => ONEWORLD
      case 2 => STAR_ALLIANCE
      case 3 => PLACEHOLDER
      case _ => throw new IllegalArgumentException("Invalid AIAlliance ID: " + id)
    }
  }
}
