package com.patson

import com.patson.data._
import com.patson.model._
import com.patson.model.airplane.Model
import com.patson.model.airplane.AirplaneConfiguration.first
import scala.util.Random
import scala.collection.mutable
import com.patson.DemandGenerator.Demand
import com.patson.model.airplane.Airplane
import com.patson.model.airplane.LinkAssignment
import com.patson.model.airplane.AirplaneConfiguration
import com.patson.init.AirlineGenerator.LinkGeneration
import com.patson.init.AirlineGenerator.LinkConfig
import com.patson.data.airplane.ModelSource

/* AI Design

- Route Management
- Pricing
- Finances
- Fleet Management

 */

object AISimulation extends App {
  private lazy val countryRelationships = CountrySource.getCountryMutualRelationships()
  private lazy val allModels = ModelSource.loadAllModels()

  def simulateAIAirlines(cycle: Int) = {
    if (cycle % 1 == 0) {
      println("AI updating this cycle")
      val aiAirlines = AirlineSource.loadAllAirlines(true).filterNot(_.aiType == AIType.PLAYER)
      val allFlightLinks = LinkSource.loadAllFlightLinks()
      val flightLinksByAirline = allFlightLinks.groupBy(_.airline.id)
      val allAirports = AirportSource.loadAllAirports(true)
      
      aiAirlines.foreach { airline =>
        if (Random.nextDouble() < 1) {
          val airlineFlightLinks = flightLinksByAirline.getOrElse(airline.id, Nil)
          val rivalFlightLinks = allFlightLinks.filterNot(_.id == airline.id)
          val linksByFromAirport = flightLinksByAirline.get(airline.id).getOrElse(List.empty).groupBy(_.from.id)
            
          //routeManagement(airline, airlineFlightLinks, flightLinksByAirline, cycle)
          //baseManagement(airline, allAirports, linksByFromAirport, cycle)
          addNewRoutes(airline, getAirlineStrategyProfile(airline), airline.getHeadQuarter().get, allAirports, rivalFlightLinks, cycle)
        }
      }
    } 
  }

  // route management

  private def routeManagement(airline: Airline, flightLinks: List[Link], flightLinksByAirline: Map[Int, List[Link]], cycle: Int) = {
    println(s"Updated routes for ${airline.name}")
    
    flightLinks.foreach {
      case flightLink : Link =>
        val linkConsumptions = LinkSource.loadLinkConsumptionsByLinkId(flightLink.id, cycle).headOption
        val rivalFlightLinks = LinkSource.loadFlightLinksByAirports(flightLink.from.id, flightLink.to.id).filterNot(_.airline.id == flightLink.airline.id)

        linkConsumptions.foreach { linkConsumption =>
          val basePrice = LinkClassValues(linkConsumption.link.price.economyVal, linkConsumption.link.price.businessVal, linkConsumption.link.price.firstVal)
            
          // all 10-week averages
          val (economyLF, businessLF, firstLF, totalLF) = computeAverageLoadFactor(flightLink, cycle)
          val (avgEconomyPrice, avgBusinessPrice, avgFirstPrice) = getAverageRivalPrice(flightLink, rivalFlightLinks)

          //println("Link: " + flightLink.from.iata + "-" + flightLink.to.iata)
          //println("Avg Load Factors: " + economyLF + "% / " + businessLF + "% / " + firstLF + "%")

          // adjusts prices down if total load factor is below 90% and no major delays or cancellations occurred
          adjustPrices(flightLink, linkConsumption, basePrice, economyLF, businessLF, firstLF)
          adjustFrequency(airline, flightLink, linkConsumption, rivalFlightLinks, flightLinksByAirline, totalLF, basePrice, cycle)
        }
    }
  }

  private def addNewRoutes(airline: Airline, airlineProfile: AirlineStrategyProfile, base: AirlineBase, allAirports: List[Airport], rivalLinks: List[Link], cycle: Int) = {
    println(s"Adding new routes for ${airline.name}")
    
    val models = allModels.filter(model => airlineProfile.modelNames.contains(model.name))
    val nearbyAirports = allAirports.filter(airport => airlineProfile.preferredCountries.contains(airport.countryCode) && airport.population > 500_000 && airport != base.airport)
    val farAirports = allAirports.filter(airport => airport.zone.split("\\|").map(_.trim).exists(airlineProfile.preferredAffinities.contains) && airport.population > 1_000_000 && airport.isGateway() && !nearbyAirports.contains(airport) & airport != base.airport)

    //println("Nearby Airports: " + nearbyAirports.map(_.name).mkString(", "))
    //println("Far Airports: " + farAirports.map(_.name).mkString(", "))

    // TODO: implement delegates when placing a new route or upgrading frequency
    val availableDelegates = airline.getDelegateInfo().availableCount
    val count = availableDelegates / 4

    println(s"${airline.name} Delegate count: ${count}")
    
    if (Random.nextDouble() < 8.0) {
      generateLinks(
        LinkGeneration(
          fromAirport = base.airport,
          toAirports = nearbyAirports,
          models = models,
          airline = airline,
          config = LinkConfig("near", nearbyAirports.size, count, 60)
        ), airline, rivalLinks, cycle
      )
    } else if (Random.nextDouble() < 0.25) {
      generateLinks(
        LinkGeneration(
          fromAirport = base.airport,
          toAirports = farAirports,
          models = models,
          airline = airline,
          config = LinkConfig("far", nearbyAirports.size, count / 2, 60)
        ), airline, rivalLinks, cycle
      )
    }
  }

  private def generateLinks(config: LinkGeneration, airline: Airline, rivalLinks: List[Link], cycle: Int) : List[Link] = {
    val airlineProfile = getAirlineStrategyProfile(airline)
    val airplaneModelsLarge = config.models.sortBy(_.capacity).reverse
    val airplaneModelsSmall = config.models.sortBy(_.capacity)

    val scoredRoutes = config.toAirports
      .filter(_.id != config.fromAirport.id)
      .filterNot(link => LinkSource.loadFlightLinkByAirportsAndAirline(config.fromAirport.id, link.id, airline.id).nonEmpty)
      .flatMap { toAirport =>
        val demand = getLinkDemand(config.fromAirport, toAirport)
        val distance = Computation.calculateDistance(config.fromAirport, toAirport)
        val targetSeats = (demand.travelerDemand.total + demand.businessDemand.total) * 2

        if (!isLinkSaturated(airline, config.fromAirport, toAirport) && targetSeats > 0 && (demand.businessDemand.total + demand.touristDemand.total + demand.travelerDemand.total) > airlineProfile.minimumRouteDemand) {
          val score = scoreNewLink(airline, airlineProfile, config.fromAirport, toAirport, rivalLinks)
          Some(ScoredRoute(toAirport, score, distance, targetSeats))
        } else {
          None
        }
      }

    val topRoutes = scoredRoutes.sortBy(-_.score).take(config.config.linkCount)
    val newLinks = topRoutes.flatMap { route => 
      println(s"Creating a new route ${config.fromAirport.iata}-${route.toAirport.iata}")
      createLink(
        fromAirport = config.fromAirport,
        toAirport = route.toAirport,
        airline = airline,
        distance = route.distance,
        targetSeats = route.targetSeats,
        modelsSmall = airplaneModelsSmall,
        modelsLarge = airplaneModelsLarge,
        rawQuality = config.config.rawQuality,
        cycle = cycle
      )  
    }

    if (newLinks.nonEmpty) {
      LinkSource.saveLinks(newLinks.filter(_.frequency > 0).toList)
    } else {
      println(s"No links on ${config.config.description} from ${config.fromAirport.iata} !!!")
    }

    newLinks
    

    /*var i = 0
    while (newLinks.length < config.config.linkCount && i < config.config.poolSize) {
      
      val pickedToAirports = drawFromPool(config.toAirports, config.config.poolSize)
      val toAirport = pickedToAirports(i)
      val existingLinks = LinkSource.loadFlightLinkByAirportsAndAirline(config.fromAirport.id, toAirport.id, airline.id)

      if (existingLinks.isEmpty && !isLinkSaturated(airline, config.fromAirport, toAirport)) {
        
        
        
        val demand = getLinkDemand(config.fromAirport, toAirport)
        val distance = Computation.calculateDistance(config.fromAirport, toAirport)
        val targetSeats = (demand.travelerDemand.total + demand.businessDemand.total) * 2

        if (targetSeats > 0 && (demand.businessDemand.total + demand.touristDemand.total + demand.travelerDemand.total) > airlineProfile.minimumRouteDemand) {
          createLink(
            fromAirport = config.fromAirport,
            toAirport = toAirport,
            airline = config.airline,
            distance = distance,
            targetSeats = targetSeats,
            modelsSmall = airplaneModelsSmall,
            modelsLarge = airplaneModelsLarge,
            rawQuality = config.config.rawQuality,
            cycle = cycle
          ).foreach(newLinks += _)
        }
      }

      i += 1
    }

    if (newLinks.nonEmpty) {
      LinkSource.saveLinks(newLinks.filter(_.frequency > 0).toList)
    } else {
      println(
        s"No links on ${config.config.description} from ${config.fromAirport.iata} !!!"
      )
    }

    newLinks.toList*/
  }

  private def createLink(fromAirport: Airport, toAirport: Airport, airline: Airline, distance: Int, targetSeats: Int, modelsSmall: List[Model], modelsLarge: List[Model], rawQuality: Int, cycle: Int): Option[Link] = {
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

        val assignedAirplanes = updateAssignedPlanes(
          model = model,
          airline = airline,
          homeAirport = fromAirport,
          frequency = frequency,
          distance = distance,
          airplanesRequired = airplanesRequired,
          maxFrequencyPerAirplane = maxFrequencyPerAirplane,
          cycle = cycle
        )

        val priceMod = if (fromAirport.popMiddleIncome < 100_000 || toAirport.popMiddleIncome < 100_000)
          1.0
        else if (fromAirport.popMiddleIncome > 1_000_000 || toAirport.popMiddleIncome > 1_000_000)
          1.0
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

        println(s"${airline.name} added a new link (${link.from.iata}-${link.to.iata}) with ${capacity} total capacity.")

        val linkCost = Computation.getLinkCreationCost(fromAirport, toAirport)
        AirlineSource.saveCashFlowItem(AirlineCashFlowItem(airline.id, CashFlowType.CREATE_LINK, linkCost * -1))
        AirlineSource.adjustAirlineBalance(airline.id, linkCost * -1)

        link.setAssignedAirplanes(assignedAirplanes)
        Some(link)
      } else {
        None
      }
    }
  }

  /* 
   * Returns whether or not the link between two Airports is saturated (meaning total capacity is greater
   * than demand and all links serving the route are sustaining high load factors)
   */
  private def isLinkSaturated(airline: Airline, fromAirport: Airport, toAirport: Airport) : Boolean = {
    val linksByAirport = LinkSource.loadFlightLinksByAirports(fromAirport.id, toAirport.id)
    val rivalLinksByAirport = linksByAirport.filterNot(_.airline.id == airline.id)
    val airlineLinksByAirport = linksByAirport.filter(_.airline.id == airline.id)
    val demand = getLinkDemand(fromAirport, toAirport)

    var totalCapacity = 0
    var totalSoldSeats = 0

    // if rival links is not empty, add up total capacity and sold seats
    if (rivalLinksByAirport.nonEmpty) rivalLinksByAirport.foreach { link => 
      totalCapacity += link.getTotalCapacity
      totalSoldSeats += link.getTotalSoldSeats
    }
    // if current airline's links are not empty, add up total capacity and sold seats
    if (airlineLinksByAirport.nonEmpty) airlineLinksByAirport.foreach { link => 
      totalCapacity += link.getTotalCapacity  
      totalSoldSeats += link.getTotalSoldSeats
    }

    if (totalCapacity < DemandGenerator.addUpDemands(demand)) return false
    else if (totalCapacity > 0 && totalSoldSeats / totalCapacity > 0.85) return true
    else return false
  }

  /* 
   * Returns the Demand of a link given the from Airport and to Airport
   */
  private def getLinkDemand(fromAirport: Airport, toAirport: Airport) : Demand = {
    val distance = Computation.calculateDistance(fromAirport, toAirport)
    val relationship = countryRelationships.getOrElse((fromAirport.countryCode, toAirport.countryCode), 0)
    val affinity = Computation.calculateAffinityValue(fromAirport.zone, toAirport.zone, relationship)
    val demand = DemandGenerator.computeBaseDemandBetweenAirports(fromAirport, toAirport, affinity, distance)
    demand
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

  /* 
   * Adjusts the price (economy, business, first all separated) of the specified flight link by 5%:
   *  - Down if the respective load factor is below a certain percentage (e.g. 90) and the current 
   *    percentage off base price is no less than 75% (to prevent prices spiraling down)
   *  - Up if the respective load factor is equal to 100%
   */
  private def adjustPrices(flightLink: Link, linkConsumption: LinkConsumptionDetails, basePrice: LinkClassValues, economyLF: Int, businessLF: Int, firstLF: Int) = {
    val basePrice = LinkClassValues(linkConsumption.link.price.economyVal, linkConsumption.link.price.businessVal, linkConsumption.link.price.firstVal)
    
    var newEconomyPrice = flightLink.price.economyVal
    var newBusinessPrice = flightLink.price.businessVal
    var newFirstPrice = flightLink.price.firstVal
      
    if (linkConsumption.link.capacity.economyVal > 0) {
      val economyPercentage = (flightLink.price.economyVal.toDouble / basePrice.economyVal.toDouble)
      if (economyLF < 60 && economyPercentage > 0.75) { newEconomyPrice = (basePrice.economyVal * (economyPercentage - 0.05)).toInt }
      if (economyLF == 100) { newEconomyPrice = (basePrice.economyVal * (economyPercentage + 0.05)).toInt }
    }

    if (linkConsumption.link.capacity.businessVal > 0) {
      val businessPercentage = (flightLink.price.businessVal.toDouble / basePrice.businessVal.toDouble)
      if (businessLF < 60 && businessPercentage > 0.75) { newBusinessPrice = (basePrice.businessVal * (businessPercentage - 0.05)).toInt }
      if (businessLF == 100) { newBusinessPrice = (basePrice.businessVal * (businessPercentage + 0.05)).toInt }
    }
              
    if (linkConsumption.link.capacity.firstVal > 0) {
      val firstPercentage = (flightLink.price.firstVal.toDouble / basePrice.firstVal.toDouble)
      if (firstLF < 60 && firstPercentage > 0.75) { newFirstPrice = (basePrice.firstVal * (firstPercentage - 0.05)).toInt }
      if (firstLF == 100) { newFirstPrice = (basePrice.firstVal * (firstPercentage + 0.05)).toInt }
    }

    if (newEconomyPrice != flightLink.price.economyVal || newBusinessPrice != flightLink.price.businessVal || newFirstPrice != flightLink.price.firstVal) {
      val newPrices = LinkClassValues(newEconomyPrice, newBusinessPrice, newFirstPrice)
      val newLink = flightLink.copy(price = newPrices)
      println(s"Updated link ${newLink.from.iata}-${newLink.to.iata} to new prices")
      LinkSource.updateLink(newLink)
    }
  }

  private def adjustFrequency(airline: Airline, flightLink: Link, linkConsumption: LinkConsumptionDetails, rivalFlightLinks: List[Link], flightLinksByAirline: Map[Int,List[Link]], totalLF: Int, basePrice: LinkClassValues, cycle: Int) = {
    val demand = getLinkDemand(flightLink.from, flightLink.to)
    var linkTotalCapacity = flightLink.getTotalCapacity
    var linkTotalSoldSeats = flightLink.getTotalSoldSeats

    if (rivalFlightLinks.size > 0) {
      rivalFlightLinks.foreach { rivalFlightLink =>
        linkTotalCapacity += rivalFlightLink.getTotalCapacity
        linkTotalSoldSeats += rivalFlightLink.getTotalSoldSeats
      }
    }

    if (totalLF >= 98 && (linkTotalSoldSeats.toDouble / linkTotalCapacity >= 0.90 || DemandGenerator.addUpDemands(demand) > linkTotalCapacity)) {
      // try increase freq
      var newFrequency = ((flightLink.frequency / 3) + 1) * 3
      if (newFrequency % 7 == 6) { newFrequency += 1 }
      else if (newFrequency % 7 == 1) { newFrequency -= 1 }
      
      if (canBaseSupportLink(airline, flightLink.copy(frequency = newFrequency), flightLinksByAirline, flightLink.from) && canAffordAirplane(airline, flightLink.getAssignedModel().get)) {
        val maxFrequencyPerAirplane = Computation.calculateMaxFrequency(flightLink.getAssignedModel().get, flightLink.distance)
        val airplanesRequired = Math.max(1, newFrequency / maxFrequencyPerAirplane)
        val assignedAirplanes = updateAssignedPlanes(flightLink.getAssignedModel().get, flightLink.airline, flightLink.from, newFrequency, flightLink.distance, airplanesRequired, maxFrequencyPerAirplane, cycle)
        val newLink = flightLink.copy(frequency = newFrequency)

        newLink.setAssignedAirplanes(assignedAirplanes)

        println(s"Updating link ${newLink.from.iata}-${newLink.to.iata} with new increased frequency")
        LinkSource.updateLink(newLink)
        LinkSource.updateAssignedPlanes(newLink.id, assignedAirplanes) 
      }
    } else if (totalLF < 90 && flightLink.price.economyVal <= (basePrice.economyVal * 0.75)) {
      // decrease freq
      if (flightLink.frequency <= 3) {
        // try and downsize plane? or delete route
        LinkSource.deleteLink(flightLink.id)
      } else {
        // reduce frequency to next multiple of 3
        var newFrequency = Math.max(3, (flightLink.frequency / 3) * 3)
        if (newFrequency % 7 == 6) { newFrequency += 1 }
        else if (newFrequency % 7 == 1) { newFrequency -= 1 }

        val maxFrequencyPerAirplane = Computation.calculateMaxFrequency(flightLink.getAssignedModel().get, flightLink.distance)
        val airplanesRequired = Math.max(1, newFrequency / maxFrequencyPerAirplane)
        val assignedAirplanes = updateAssignedPlanes(flightLink.getAssignedModel().get, flightLink.airline, flightLink.from, newFrequency, flightLink.distance, airplanesRequired, maxFrequencyPerAirplane, cycle)
        val newLink = flightLink.copy(frequency = newFrequency)

        newLink.setAssignedAirplanes(assignedAirplanes)

        println(s"Updating link ${newLink.from.iata}-${newLink.to.iata} with new reduced frequency")
        LinkSource.updateLink(newLink)
        LinkSource.updateAssignedPlanes(newLink.id, assignedAirplanes)
      }
    }
  }

  private def computeAverageLoadFactor(link: Link, cycle: Int) : (Int, Int, Int, Int) = {
    var cycleCount = 10
    // if current cycle is less than cycleCount, only check back the amount of current cycles
    if(cycle < cycleCount) { cycleCount = cycle }
    
    var totalEconomySold = 0
    var totalEconomyCapacity = 0
    var totalBusinessSold = 0
    var totalBusinessCapacity = 0
    var totalFirstSold = 0
    var totalFirstCapacity = 0

    val linkConsumptions = LinkSource.loadLinkConsumptionsByLinkId(link.id, cycleCount)
    linkConsumptions.foreach { linkConsumption =>
      totalEconomySold += linkConsumption.link.soldSeats.economyVal
      totalEconomyCapacity += linkConsumption.link.capacity.economyVal

      totalBusinessSold += linkConsumption.link.soldSeats.businessVal
      totalBusinessCapacity += linkConsumption.link.capacity.businessVal

      totalFirstSold += linkConsumption.link.soldSeats.firstVal
      totalFirstCapacity += linkConsumption.link.capacity.firstVal        
    }

    val economyLoadFactor = if (totalEconomyCapacity > 0) ((totalEconomySold.toDouble / totalEconomyCapacity) * 100).toInt else 0
    val businessLoadFactor = if (totalBusinessCapacity > 0) ((totalBusinessSold.toDouble / totalBusinessCapacity) * 100).toInt else 0
    val firstLoadFactor = if (totalFirstCapacity > 0) ((totalFirstSold.toDouble / totalFirstCapacity) * 100).toInt else 0

    val totalSold = totalEconomySold + totalBusinessSold + totalFirstSold
    val totalCapacity = totalEconomyCapacity + totalBusinessCapacity + totalFirstCapacity
    val totalLoadFactor = if (totalCapacity > 0) ((totalSold.toDouble / totalCapacity) * 100).toInt else 0

    (economyLoadFactor, businessLoadFactor, firstLoadFactor, totalLoadFactor)
  }

  private def getAverageRivalPrice(link: Link, rivalLinks: List[Link]) : (Int, Int, Int) = {
    var economyPrice = 0
    var businessPrice = 0
    var firstPrice = 0
    var avgEconomyPrice = 0
    var avgBusinessPrice = 0
    var avgFirstPrice = 0

    if (rivalLinks.size > 0) {
      rivalLinks.foreach { rivalLink =>
        economyPrice += rivalLink.price.economyVal
        businessPrice += rivalLink.price.businessVal
        firstPrice += rivalLink.price.firstVal
      }

      avgEconomyPrice = economyPrice / rivalLinks.size
      avgBusinessPrice = businessPrice / rivalLinks.size
      avgFirstPrice = firstPrice / rivalLinks.size
    }

    (avgEconomyPrice, avgBusinessPrice, avgFirstPrice)
  }

  private def getTotalRivalCapacity(link: Link, rivalLinks: List[Link]) : (Int, Int, Int) = {
    var totalEconomyCapacity = 0
    var totalBusinessCapacity = 0
    var totalFirstCapacity = 0

    if (rivalLinks.size > 0) {
      rivalLinks.foreach { rivalLink =>
        totalEconomyCapacity += rivalLink.capacity.economyVal
        totalBusinessCapacity += rivalLink.capacity.businessVal
        totalFirstCapacity += rivalLink.capacity.firstVal
      }
    }

    (totalEconomyCapacity, totalBusinessCapacity, totalFirstCapacity)
  }

  private def isCompetition(link: Link, rivalLinks: List[Link]) : Boolean = {
    if (rivalLinks.isEmpty) { return false } else return true
  }

  // fleet management

  private def updateAssignedPlanes(model: Model, airline: Airline, homeAirport: Airport, frequency: Int, distance: Int, airplanesRequired: Int, maxFrequencyPerAirplane: Int, cycle: Int) : Map[Airplane, LinkAssignment] = {
    val assignedAirplanes = mutable.Map[Airplane, LinkAssignment]()
    val flightMinutesRequired = Computation.calculateFlightMinutesRequired(model, distance)
    var remainingFrequency = frequency
    val allAirplanes = AirplaneSource.loadAirplanesByOwner(airline.id)
    val availablePlanesOfModel = allAirplanes.filter(_.model == model).toBuffer
    // Map to track each plane's remaining minutes
    val airplaneRemainingMinutes = availablePlanesOfModel.map { airplane =>
      airplane -> (airplane.availableFlightMinutes)
    }.filter(_._2 >= flightMinutesRequired) // Only keep usable planes

    val sortedPlanes = airplaneRemainingMinutes.sortBy(-_._2).toBuffer // Prefer planes with most free time

    for (_ <- 0 until airplanesRequired if remainingFrequency > 0) {
      val (selectedPlane, availableMinutes) = if (sortedPlanes.nonEmpty) {
        sortedPlanes.remove(0)
      } else {
        // No existing plane available — buy new
        val newAirplane = createAirplane(model, airline, homeAirport, cycle)
        (newAirplane, Airplane.MAX_FLIGHT_MINUTES)
      }

      val frequencyForThis = math.min(remainingFrequency, Math.floor(availableMinutes / flightMinutesRequired).toInt)
      val flightMinutesForThis = frequencyForThis * flightMinutesRequired

      assignedAirplanes.put(selectedPlane, LinkAssignment(frequencyForThis, flightMinutesForThis))

      remainingFrequency -= frequencyForThis
    }

    assignedAirplanes.toMap
  }

  private def createAirplane(model: Model, airline: Airline, homeAirport: Airport, cycle: Int): Airplane = {
    val airplane = Airplane(
      model = model,
      owner = airline,
      constructedCycle = cycle,
      purchasedCycle = cycle,
      condition = Airplane.MAX_CONDITION,
      depreciationRate = 0,
      value = model.price,
      home = homeAirport,
      configuration = AirplaneConfiguration.empty
    )

    val cost = airplane.model.price
    AirlineSource.adjustAirlineBalance(airline.id, cost * -1)
    AirlineSource.saveCashFlowItem(AirlineCashFlowItem(airline.id, CashFlowType.BUY_AIRPLANE, cost * -1))

    airplane.assignDefaultConfiguration()
    AirplaneSource.saveAirplanes(List(airplane))
    airplane
  }

  private def canAffordAirplane(airline: Airline, model: Model) : Boolean = {
    if (airline.airlineInfo.balance - model.price > 1000000) {
      return true
    }
    else return false
  }

  // base management
  
  private def baseManagement(airline: Airline, allAirports: List[Airport], linksByFromAirport: Map[Int, List[Link]], cycle: Int) = {
    println(s"Managing bases for ${airline.name}")
    
    val bases = airline.getBases()

    if (allBasesAtCapacity(bases, airline, linksByFromAirport)) {
      // upgrade bases or expand to a new base
      val airlineCashFlow = CashFlowSource.loadCashFlowByAirline(airline.id, cycle, Period.WEEKLY)
      val cashFlow = if (airlineCashFlow.isDefined) airlineCashFlow.get.cashFlow else 0

      val baseToUpgrade = getLowestScaleBase(bases)
      if (canUpgradeBase(airline, baseToUpgrade, cashFlow)) {
        val upgradeCost = baseToUpgrade.calculateUpgradeCost(baseToUpgrade.scale + 1)
        AirlineSource.saveCashFlowItem(AirlineCashFlowItem(airline.id, CashFlowType.BASE_CONSTRUCTION, upgradeCost * -1))
        AirlineSource.saveAirlineBase(baseToUpgrade.copy(scale = (baseToUpgrade.scale + 1)))
        println(s"Base at ${baseToUpgrade.airport.iata} upgraded to scale ${baseToUpgrade.scale}")
      } else {
        // expand new base; shouldn't expand to new bases if less than 12 current bases
        if (bases.size < 12) {
          val newAirportBase = getNewExpansionBase(airline, getAirlineStrategyProfile(airline), allAirports)
          val base = AirlineBase(airline, newAirportBase, newAirportBase.countryCode, 1, cycle, false)
          val upgradeCost = base.calculateUpgradeCost(1)
          AirlineSource.saveCashFlowItem(AirlineCashFlowItem(airline.id, CashFlowType.BASE_CONSTRUCTION, upgradeCost * -1))
          AirlineSource.saveAirlineBase(base)
          println(s"New base opened at ${base.airport.iata}")
        }
      }
    }
  }

  private def getNewExpansionBase(airline: Airline, airlineProfile: AirlineStrategyProfile, allAirports: List[Airport]) : Airport = {
    val airportsFiltered = allAirports.filter { airport =>
      airport.population > 500000 &&
      (airlineProfile.preferredCountries.contains(airport.countryCode) || airlineProfile.preferredAffinities.contains(airport.zone))  
    }

    var newAirportBase = airportsFiltered.head
    var newAirportBaseScore = 0.0

    airportsFiltered.foreach { airport => 
      if (scoreAirportAsBase(airline, airport, airlineProfile) > newAirportBaseScore) {
        newAirportBase = airport
        newAirportBaseScore = scoreAirportAsBase(airline, airport, airlineProfile)
      }
    }

    newAirportBase
  }

  private def canUpgradeBase(airline: Airline, base: AirlineBase, cashFlow: Long) : Boolean = {
    val multiplier = base.scale match {
      case 1 | 2 => 1
      case 3 | 4 => 2
      case 5 | 6 => 3
      case 7 | 8 => 5
      case 9 | 10 => 8
      case 11 | 12 => 12
      case _ => 14
    }

    val safeSpendingLimit = cashFlow * 6 * multiplier
    val upgradeCost = base.calculateUpgradeCost(base.scale + 1)

    if ((airline.getBalance() - upgradeCost > 1000000) && safeSpendingLimit > upgradeCost) return true
    return false
  }

  private def getLowestScaleBase(bases: List[AirlineBase]) : AirlineBase = {
    var lowestScaleBase = getHeadquarter(bases)
    bases.foreach { base =>
      if (base.scale < lowestScaleBase.scale) lowestScaleBase = base
    }

    lowestScaleBase
  }

  private def isBaseMaxCapacity(airline: Airline, base: AirlineBase, linksByFromAirport: Map[Int, List[Link]]) : Boolean = {
    val linksFromBase = linksByFromAirport.get(base.airport.id)
    val currentBaseStaff = getCurrentBaseStaffRequired(airline, base, linksByFromAirport)
    if (linksFromBase.nonEmpty && currentBaseStaff > (base.getOfficeStaffCapacity - 15)) {
      return true
    }

    return false
  }

  private def allBasesAtCapacity(bases: List[AirlineBase], airline: Airline, linksByFromAirport: Map[Int, List[Link]]) : Boolean = {
    bases.foreach { base =>
      if (!isBaseMaxCapacity(airline, base, linksByFromAirport)) {
        return false
      }
    }

    return true
  }
  
  private def canBaseSupportLink(airline: Airline, link: Link, flightLinksByAirline: Map[Int, List[Link]], homeAirport: Airport) : Boolean = {
    val base = airline.getBases().find(_.airport == homeAirport).get
    val linksByFromAirport = flightLinksByAirline.get(airline.id).getOrElse(List.empty).groupBy(_.from.id)
    val currentBaseStaff = getCurrentBaseStaffRequired(airline, base, linksByFromAirport)

    if (base.airport == homeAirport && (link.getCurrentOfficeStaffRequired + currentBaseStaff) <= base.getOfficeStaffCapacity) {
      return true
    }

    return false
  }

  private def getCurrentBaseStaffRequired(airline: Airline, base: AirlineBase, linksByFromAirport: Map[Int, List[Link]]) : Int = {
    val currentBaseStaff = linksByFromAirport.get(base.airport.id) match {
      case Some(links) => links.map(_.getCurrentOfficeStaffRequired).sum
      case None => 0
    }

    currentBaseStaff
  }

  private def getHeadquarter(bases: List[AirlineBase]) : AirlineBase = {
    bases.foreach { base =>
      if (base.headquarter) return base
    }

    return bases.head
  }


  // decision tree logic

  private def scoreAirportAsBase(airline: Airline, airport: Airport, airlineProfile: AirlineStrategyProfile) : Double = {
    val isDomestic = airline.getCountryCode().get == airport.countryCode
    var isRegional = false

    airlineProfile.preferredAffinities.foreach { affinity =>
      if (airport.zone.contains(affinity)) isRegional = true
    }

    val baseWeight = 100
    val populationScore = airport.population / 100_000_000.0
    val incomeScore = airport.income / 100_000.0
    val competitionScore = 1.0 - (AirportRating.rateAirport(airport).competitionRating.toDouble / 100)
    val relationshipScore = 1.0

    val geoWeight = 
      if (isDomestic) airlineProfile.domesticBasePriority
      else if (isRegional) airlineProfile.regionBasePriority
      else airlineProfile.globalBasePriority

    baseWeight * populationScore * incomeScore * geoWeight * competitionScore * relationshipScore
  }

  private def scoreNewLink(airline: Airline, airlineProfile: AirlineStrategyProfile, fromAirport: Airport, toAirport: Airport, rivalLinks: List[Link]) : Double = {
    val demand = getLinkDemand(fromAirport, toAirport)
    val rivalLinksOnLink = rivalLinks.filter { link =>
      link.from == fromAirport && link.to == toAirport && link.airline.id != airline.id
    }
    var totalExistingCapacity = 0
    rivalLinksOnLink.foreach { link => 
      totalExistingCapacity += link.getTotalCapacity
    }

    val baseWeight = 100
    val populationScore = fromAirport.population / 100_000_000.0
    val incomeScore = fromAirport.income / 100_000.0
    val competitionScore =
      if (totalExistingCapacity > DemandGenerator.addUpDemands(demand)) 0.0
      else totalExistingCapacity.toDouble / DemandGenerator.addUpDemands(demand)
    val relationshipScore = 1.0

    baseWeight * populationScore * incomeScore * competitionScore * relationshipScore
  }

  case class ScoredRoute(toAirport: Airport, score: Double, distance: Int, targetSeats: Int)

  case class AirlineStrategyProfile(
    preferredDomestic: Boolean,
    preferredCountries: List[String],
    preferredAffinities: List[String],
    domesticBasePriority: Double,
    regionBasePriority: Double,
    globalBasePriority: Double,
    minimumRouteDemand: Int,
    modelNames: List[String]
  )

  private def getAirlineStrategyProfile(airline: Airline) : AirlineStrategyProfile = {
    val airlineProfile = airline.name match {
      case "Delta Air Lines" => AirlineStrategyProfile(true, List("US"), List("Anglophone"), 1.0, 0.0, 0.0, 500, List("Boeing 737-800", "Boeing 737-900ER", "Boeing 767-300", "Airbus A350-900"))
      case "United Airlines" => AirlineStrategyProfile(true, List("US"), List("Anglophone"), 1.0, 0.0, 0.0, 500, List("Boeing 737-800", "Boeing 737-900ER", "Boeing 737 MAX 8", "Boeing 767-300", "Boeing 787-9 Dreamliner"))
      case "American Airlines" => AirlineStrategyProfile(true, List("US"), List("Anglophone"), 1.0, 0.0, 0.0, 500, List("Boeing 737-800", "Boeing 737 MAX 8", "Boeing 777-200", "Boeing 787-9 Dreamliner"))
    }
    airlineProfile
  }
}
