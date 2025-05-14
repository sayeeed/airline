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
    
    // Generate AI Airlines
    generateAIAirline("Delta Air Lines", "delta")
    generateAIAirline("American Airlines", "american")
    generateAIAirline("United Airlines", "united")
    generateAIAirline("Southwest Airlines", "southwest")
    generateAIAirline("JetBlue Airways", "jetblue")
    generateAIAirline("Alaska Airlines", "alaska")
    generateAIAirline("Spirit Airlines", "spirit")
    generateAIAirline("Frontier Airlines", "frontier")
    generateAIAirline("Hawaiian Airlines", "hawaiian")
    generateAIAirline("Allegiant Air", "allegiant")

    /*
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
    generateIndigo(List("IN"))
    generateAirIndia(List("IN"))

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
    //generateLatamAirline()
    generateArabiaAirline()
    generateCaribbeanAirline()
    generatePacificAirline()
    */

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
    val toAirports = airports.filter(airport => profile.preferredCountries.contains(airport.countryCode))

    val user = createUser(username)
    val airline = createAIAirline(name, hq, profile.airlineType, profile.aiType, profile.targetServiceQuality)
    
    println(s"generating $name at ${hq.iata} with ${profile.airlineType} profile")
    println(s"Bases: ${bases}")
    println(s"To Airports: ${toAirports}")

    AirlineSource.saveAirlines(List(airline))
    UserSource.setUserAirline(user, airline)
    AirlineSource.saveAirlineInfo(airline, false)
    AirlineSource.saveAirplaneRenewal(airline.id, 50)

    makeBase(airline, hq, true)
    generateLinksForAirline(airline, hq, toAirports, profile.modelNames, profile.linkMaxDistance + 1000, profile.maxLinksPerBase, profile.maxLongLinksPerBase, profile.routeServiceLevel)
    bases.zipWithIndex.foreach { case (baseAirport, index) => makeBase(airline, baseAirport) }
    bases.foreach( airport => generateLinksForAirline(airline, airport, toAirports, profile.modelNames, profile.linkMaxDistance + 1000, profile.maxLinksPerBase, profile.maxLongLinksPerBase, profile.routeServiceLevel))
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
    airline.setReputation(30)
    airline.setSkipTutorial(true)
    airline.setCountryCode(hqAirport.countryCode)
    airline.setAirlineCode(airline.getDefaultAirlineCode())
    airline
  }

  private def generateLinksForAirline(airline: Airline, baseAirport: Airport, toAirports: List[Airport], modelNames: List[String], maxDistance: Int, maxLinksPerBase: Int, maxLongLinksPerBase: Int, standardRouteQuality: Int): Unit = {
    println("Generating links...")
    val models = allModels.filter(model => modelNames.contains(model.name))
    val nearbyFocusAirports = findAirports(toAirports, baseAirport, maxDistance / 2)
    val farAirports = findAirports(airports, baseAirport, maxDistance, maxDistance / 4).filterNot(airport => nearbyFocusAirports.contains(airport))
    bufferOfAirplanes.clear()

    generateLinks(
      LinkGeneration(
        fromAirport = baseAirport,
        toAirports = nearbyFocusAirports,
        models = models,
        airline = airline,
        config = LinkConfig("near", nearbyFocusAirports.size, Math.min(maxLinksPerBase, nearbyFocusAirports.size), standardRouteQuality)
      )
    )

    generateLinks(
      LinkGeneration(
        fromAirport = baseAirport,
        toAirports = farAirports,
        models = models,
        airline = airline,
        config = LinkConfig("far", farAirports.size, maxLongLinksPerBase, Math.min(100, standardRouteQuality + 20))
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
    val airports = config.toAirports.takeRight(config.config.poolSize + 1)

    val airplaneModelsLarge = config.models.sortBy(_.capacity).reverse
    val airplaneModelsSmall = config.models.sortBy(_.capacity)
    val newLinks = ListBuffer[Link]()

    var i = 0
    while (newLinks.length < config.config.linkCount && i < config.config.poolSize) {
      val pickedToAirports = drawFromPool(airports, config.config.poolSize)
      val toAirport = pickedToAirports(i)

      val distance = Computation.calculateDistance(config.fromAirport, toAirport)
      val relationship = countryRelationships.getOrElse((config.fromAirport.countryCode, toAirport.countryCode), 0)

      val affinity = Computation.calculateAffinityValue(config.fromAirport.zone, toAirport.zone, relationship)

      val demand = DemandGenerator.computeBaseDemandBetweenAirports(config.fromAirport, toAirport, affinity, distance)
      val targetSeats = (demand.travelerDemand.total + demand.businessDemand.total) * 2

      if (targetSeats > 0 && (demand.businessDemand.total + demand.touristDemand.total + demand.travelerDemand.total) > 200) {
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

      i += 1
    }

    if (newLinks.nonEmpty) {
      LinkSource.saveLinks(newLinks.filter(_.frequency > 0).distinct.toList)
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
}
