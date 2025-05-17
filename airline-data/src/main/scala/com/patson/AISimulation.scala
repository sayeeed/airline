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
import com.patson.ai.NegotiationUtil
import com.patson.ai.AISimUtil
import com.patson.ai.AIAirlines

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
      val allAirports = AirportSource.loadAllAirports(true, true)
      val allConsumptions = LinkSource.loadLinkConsumptions()
      var cycleCount = 10
      // if current cycle is less than cycleCount, only check back the amount of current cycles
      if(cycle < cycleCount) { cycleCount = cycle }
      val allConsumptionsHistory = LinkSource.loadLinkConsumptions(cycleCount)
      val linksByAirportPair: Map[(Int,Int), List[Link]] = allFlightLinks.groupBy(link => AISimUtil.airportPairKey(link.from.id, link.to.id))
      
      aiAirlines.foreach { airline =>
        if (Random.nextDouble() < 1) {
          val airlineFlightLinks = flightLinksByAirline.getOrElse(airline.id, Nil)
          val rivalFlightLinks = allFlightLinks.filterNot(_.id == airline.id)
          val linksByFromAirport = flightLinksByAirline.get(airline.id).getOrElse(List.empty).groupBy(_.from.id)
          val airlineConsumptions = allConsumptions.filter(_.link.airline.id == airline.id).groupBy(_.link.id)
          val airlineConsumptionsHistory = allConsumptionsHistory.filter(_.link.airline.id == airline.id).groupBy(_.link.id)
          val linksByAirline = flightLinksByAirline.get(airline.id).getOrElse(List.empty)
          val linksByAirlineAndAirportPair: Map[(Int,Int), List[Link]] = allFlightLinks.filter(_.airline.id == airline.id).groupBy(link => AISimUtil.airportPairKey(link.from.id, link.to.id))

          val t1 = System.nanoTime()
          routeManagement(airline, airlineFlightLinks, linksByAirline, airlineConsumptions, rivalFlightLinks, airlineConsumptionsHistory, linksByAirlineAndAirportPair, cycle)
          val t2 = System.nanoTime()
          baseManagement(airline, allAirports, linksByFromAirport, cycle)
          val t3 = System.nanoTime()
          addNewRoutes(airline, AIAirlines.getAirlineStrategyProfile(airline.name), allAirports, rivalFlightLinks, linksByFromAirport, flightLinksByAirline, linksByAirline, linksByAirportPair, linksByAirlineAndAirportPair, cycle)
          val t4 = System.nanoTime()

          println(s"[${airline.name}] route=${(t2-t1)/1e6}ms base=${(t3-t2)/1e6}ms newRoutes=${(t4-t3)/1e6}ms")
        }
      }
    } 
  }

  /* 
  
    ROUTE MANAGMENT

   */

  private def routeManagement(airline: Airline, flightLinks: List[Link], linksByAirline: List[Link], airlineConsumptions: Map[Int, List[LinkConsumptionDetails]], rivalFlightLinks: List[Link], airlineConsumptionsHistory: Map[Int, List[LinkConsumptionDetails]], linksByAirlineAndAirportPair: Map[(Int,Int), List[Link]], cycle: Int) = {
    println(s"Updated routes for ${airline.name}")

    val rivalFlightLinksByRoute = rivalFlightLinks.groupBy(link => (link.from.id, link.to.id))
    val updatedLinks = mutable.ListBuffer[Link]()
    val freqAdjustedLinks = mutable.ListBuffer[FrequencyAdjustment]()

    flightLinks.foreach {
      case link : Link =>
        //val linkConsumptions = LinkSource.loadLinkConsumptionsByLinkId(flightLink.id, cycle).headOption
        val linkConsumption = airlineConsumptions.get(link.id).flatMap(_.headOption)
        val linkConsumptionsHistory = airlineConsumptionsHistory.getOrElse(link.id, Nil)
        val rivalLinksOnLink = rivalFlightLinksByRoute.getOrElse((link.from.id, link.to.id), Nil)

        if (linkConsumption.isDefined) {
          val basePrice = LinkClassValues(linkConsumption.get.link.price.economyVal, linkConsumption.get.link.price.businessVal, linkConsumption.get.link.price.firstVal)
              
          // all 10-week averages
          val (economyLF, businessLF, firstLF, totalLF) = AISimUtil.computeAverageLoadFactor(link, linkConsumptionsHistory, cycle)
          val (avgEconomyPrice, avgBusinessPrice, avgFirstPrice) = AISimUtil.getAverageRivalPrice(link, rivalLinksOnLink)
  
          // adjusts prices down if total load factor is below 90% and no major delays or cancellations occurred
          updatedLinks ++= adjustPrices(link, linkConsumption.get, basePrice, economyLF, businessLF, firstLF).toList
          adjustFrequency(airline, link, linkConsumption.get, rivalLinksOnLink, linksByAirline, totalLF, basePrice, linksByAirlineAndAirportPair, cycle).foreach(freqAdjustedLinks += _)
        }
    }

    if (updatedLinks.nonEmpty) {
      LinkSource.updateLinks(updatedLinks.result())
    }
    if (freqAdjustedLinks.nonEmpty) {
      LinkSource.updateLinks(freqAdjustedLinks.map(_.link).toList)
      freqAdjustedLinks.foreach(adj => LinkSource.updateAssignedPlanes(adj.link.id, adj.assignedAirplanes))
    }
  }

  private def addNewRoutes(airline: Airline, airlineProfile: AIAirlines.AirlineStrategyProfile, allAirports: List[Airport], rivalLinks: List[Link], linksByFromAirport: Map[Int, List[Link]], flightLinksByAirline: Map[Int, List[Link]], linksByAirline: List[Link], linksByAirportPair: Map[(Int,Int), List[Link]], linksByAirlineAndAirportPair: Map[(Int,Int), List[Link]], cycle: Int): Unit = {
    println(s"Adding new routes for ${airline.name}")
    
    val models = allModels.filter(model => airlineProfile.modelNames.contains(model.name))
    val bases = airline.getBases()

    val base = AISimUtil.getLowestCapacityBase(bases, airline, linksByFromAirport)

    val airportZonesMap: Map[Int, Set[String]] = allAirports.map(a => a.id -> a.zone.split("\\|").map(_.trim).toSet).toMap

    // PRIMARY AIRPORTS
    val primaryToAirports = allAirports.filter { airport => 
      val notSmallAirport = airport.size > 3
      val isPrimaryCountry = airlineProfile.primaryCountriesServed.contains(airport.countryCode)
      val isPrimaryAffinity = airportZonesMap(airport.id).exists(airlineProfile.primaryAffinitiesServed.contains)
      notSmallAirport && (isPrimaryCountry || isPrimaryAffinity)
    }
    val primaryAirportIDs = primaryToAirports.map(_.id).toSet
    // SECONDARY AIRPORTS
    val secondaryToAirports = allAirports.filter { airport => 
      val notSmallAirport = airport.size > 3
      val isSecondaryCountry = airlineProfile.secondaryCountriesServed.contains(airport.countryCode)
      val isSecondaryAffinity = airportZonesMap(airport.id).exists(airlineProfile.secondaryAffinitiesServed.contains)
      val isNotPrimaryAirport = !primaryAirportIDs.contains(airport.id)
      notSmallAirport && (isSecondaryCountry || isSecondaryAffinity) && isNotPrimaryAirport
    }
    val secondaryAirportIDs = secondaryToAirports.map(_.id).toSet
    // TERTIARY AIRPORTS (base specific airports with high affinity matches)
    val baseSpecificAirports = allAirports.filter { airport =>
      val notSmallAirport = airport.size > 3
      val relationship = CountrySource.getCountryMutualRelationship(base.airport.countryCode, airport.countryCode)
      val affinity = Computation.calculateAffinityValue(base.airport.zone, airport.zone, relationship)
      val isNotPrimaryAirport = !primaryAirportIDs.contains(airport.id)
      val isNotSecondaryAirport = !secondaryAirportIDs.contains(airport.id)
       notSmallAirport && affinity > 1 && isNotPrimaryAirport && isNotSecondaryAirport
    }

    // secondary plus tertiary airports
    val modifiedSecondaryToAirports = (secondaryToAirports ++ baseSpecificAirports)

    val existingRoutes = flightLinksByAirline.getOrElse(airline.id, Nil).map(link => (link.from.id, link.to.id)).toSet

    val primaryLinkGeneration = LinkGeneration(
      fromAirport = base.airport,
      toAirports = primaryToAirports,
      models = models,
      airline = airline,
      config = LinkConfig("near", primaryToAirports.size, 1, airlineProfile.routeServiceLevel)
    )
    val secondaryLinkGeneration = LinkGeneration(
      fromAirport = base.airport,
      toAirports = modifiedSecondaryToAirports,
      models = models,
      airline = airline,
      config = LinkConfig("far", modifiedSecondaryToAirports.size, 1, airlineProfile.routeServiceLevel)
    )

    var attempts = 0
    var delegateAvailable = airline.getDelegateInfo().availableCount > 1
    var hasSufficientFunds = airline.airlineInfo.balance > 50_000_000
    if (!delegateAvailable || !hasSufficientFunds) return

    while (delegateAvailable && hasSufficientFunds && attempts < 6) {
      // Try and generate close/local routes before trying long-range routes.
      val newLinks = if (Random.nextDouble() < airlineProfile.primaryServedRatio) {
        generateLinks(primaryLinkGeneration, airline, rivalLinks, linksByAirline, existingRoutes, linksByAirportPair, linksByAirlineAndAirportPair, cycle)
      } else {
        generateLinks(secondaryLinkGeneration, airline, rivalLinks, linksByAirline, existingRoutes, linksByAirportPair, linksByAirlineAndAirportPair, cycle)
      }
      attempts += 1

      delegateAvailable = airline.getDelegateInfo().availableCount > 1
      hasSufficientFunds = airline.airlineInfo.balance > 50_000_000
    }
  }

  private def generateLinks(config: LinkGeneration, airline: Airline, rivalLinks: List[Link], linksByAirline: List[Link], existingRoutes: Set[(Int, Int)], linksByAirportPair: Map[(Int,Int), List[Link]], linksByAirlineAndAirportPair: Map[(Int,Int), List[Link]], cycle: Int) : List[Link] = {
    val airlineProfile = AIAirlines.getAirlineStrategyProfile(airline.name)
    val sortedModels = config.models.sortBy(_.capacity)
    val airplaneModelsLarge = sortedModels.reverse
    val airplaneModelsSmall = sortedModels

    val scoredRoutes = config.toAirports
      .filter(_.id != config.fromAirport.id)
      .filterNot(to => existingRoutes.contains((config.fromAirport.id, to.id)))
      .flatMap { toAirport =>
        val demand = AISimUtil.getLinkDemand(config.fromAirport, toAirport)
        val totalDemand = DemandGenerator.addUpDemands(demand)
        val distance = Computation.calculateDistance(config.fromAirport, toAirport)
        val targetSeats = (demand.travelerDemand.total + demand.businessDemand.total) * 2
        val linkCost = Computation.getLinkCreationCost(config.fromAirport, toAirport)

        if (!AISimUtil.isLinkSaturated(airline, config.fromAirport, toAirport, linksByAirportPair) && targetSeats > 0 && totalDemand > airlineProfile.minimumRouteDemand && linkCost < airline.airlineInfo.balance) {
          val score = scoreNewLink(airline, airlineProfile, config.fromAirport, toAirport, rivalLinks)
          Some(ScoredRoute(toAirport, score, distance, targetSeats))
        } else {
          None
        }
      }

    val topRoutes = scoredRoutes.sortBy(-_.score).take(config.config.linkCount)
    var newLinks = List[Link]()
    var tryNegotiateLink = true
    var attempts = 0
    println(s"Trying to generate links with top routes: ${topRoutes.size}")
    
    while (tryNegotiateLink && attempts <= 12) {
      newLinks = topRoutes.flatMap { route => 
        createLink(
          fromAirport = config.fromAirport,
          toAirport = route.toAirport,
          airline = airline,
          distance = route.distance,
          targetSeats = route.targetSeats,
          modelsSmall = airplaneModelsSmall,
          modelsLarge = airplaneModelsLarge,
          rawQuality = config.config.rawQuality,
          cycle = cycle,
          linksByAirline = linksByAirline,
          linksByAirlineAndAirportPair = linksByAirlineAndAirportPair
        )
      }

      if (newLinks.nonEmpty) tryNegotiateLink = false
      attempts += 1
    }

    if (newLinks.nonEmpty) {
      LinkSource.saveLinks(newLinks.filter(_.frequency > 0).toList)
    } else {
      //println(s"No links on ${config.config.description} from ${config.fromAirport.iata} !!!")
    }

    newLinks
  }

  private def createLink(fromAirport: Airport, toAirport: Airport, airline: Airline, distance: Int, targetSeats: Int, modelsSmall: List[Model], modelsLarge: List[Model], rawQuality: Int, cycle: Int, linksByAirline: List[Link], linksByAirlineAndAirportPair: Map[(Int,Int), List[Link]]): Option[Link] = {
    // selects the model to fly the link
    val linkCost = Computation.getLinkCreationCost(fromAirport, toAirport)
    /*val pickedModel = modelsSmall.find(model =>
        model.capacity * Computation.calculateMaxFrequency(
          model,
          distance
        ) >= targetSeats && model.range >= distance && toAirport.runwayLength >= model.runwayRequirement
      ).orElse(modelsLarge.find(model => model.range >= distance && model.runwayRequirement <= toAirport.runwayLength))*/

    val pickedModel = (modelsSmall ++ modelsLarge)
      .find(model =>
        model.range >= distance &&
        model.runwayRequirement <= toAirport.runwayLength &&
        (model.capacity * Computation.calculateMaxFrequency(model, distance) >= targetSeats || modelsSmall.contains(model))
      )

    pickedModel.flatMap { model =>
      val rawFrequency = targetSeats.toDouble / model.capacity
      val closestMultipleOf7 = (Math.ceil(rawFrequency / 7.0) * 7).toInt
      val frequency = Math.min(closestMultipleOf7, 35)
    
      // If there is frequency and the airline can afford buying airplanes and the link creation cost, go forward with creating the link
      if (frequency > 0 && canAffordAirplane(airline, pickedModel.get) && linkCost <= airline.airlineInfo.balance) {
        val maxFrequencyPerAirplane = Computation.calculateMaxFrequency(model, distance)
        val airplanesRequired = math.max(1, frequency / maxFrequencyPerAirplane)

        // add planes to the route. This will also buy planes if needed.
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

        // set prices
        val flightCategory = Computation.getFlightCategory(fromAirport, toAirport)
        val baseIncome = fromAirport.baseIncome
        val econPrice = (Pricing.computeStandardPrice(distance, flightCategory, ECONOMY, PassengerType.BUSINESS, baseIncome)).toInt
        val bizPrice = (Pricing.computeStandardPrice(distance, flightCategory, BUSINESS, PassengerType.BUSINESS, baseIncome)).toInt
        val firstPrice = (Pricing.computeStandardPrice(distance, flightCategory, FIRST, PassengerType.BUSINESS, baseIncome)).toInt

        val duration = Computation.calculateDuration(model, distance)
        val capacity = AISimUtil.calculateTotalCapacity(assignedAirplanes)

        // create the link
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

        if (AISimUtil.canBaseSupportLink(airline, link, linksByAirline, fromAirport)) {
          // negotiation
          Some(AISimUtil.negotiateLink(airline, link, linksByAirlineAndAirportPair, cycle))
        } else {
          None
        }
      }
      None
    }
  }

  /* 
   * Adjusts the price (economy, business, first all separated) of the specified flight link by 5%:
   *  - Down if the respective load factor is below a certain percentage (e.g. 90) and the current 
   *    percentage off base price is no less than 75% (to prevent prices spiraling down)
   *  - Up if the respective load factor is equal to 100%
   */
  private def adjustPrices(link: Link, linkConsumption: LinkConsumptionDetails, basePrice: LinkClassValues, economyLF: Int, businessLF: Int, firstLF: Int): mutable.ListBuffer[Link] = {    
    var newEconomyPrice = link.price.economyVal
    var newBusinessPrice = link.price.businessVal
    var newFirstPrice = link.price.firstVal
    val newLinks = mutable.ListBuffer[Link]()
      
    if (linkConsumption.link.capacity.economyVal > 0) {
      val economyPercentage = (link.price.economyVal.toDouble / basePrice.economyVal.toDouble)
      if (economyLF < 60 && economyPercentage > 0.75) { newEconomyPrice = (basePrice.economyVal * (economyPercentage - 0.05)).toInt }
      if (economyLF == 100) { newEconomyPrice = (basePrice.economyVal * (economyPercentage + 0.05)).toInt }
    }

    if (linkConsumption.link.capacity.businessVal > 0) {
      val businessPercentage = (link.price.businessVal.toDouble / basePrice.businessVal.toDouble)
      if (businessLF < 60 && businessPercentage > 0.75) { newBusinessPrice = (basePrice.businessVal * (businessPercentage - 0.05)).toInt }
      if (businessLF == 100) { newBusinessPrice = (basePrice.businessVal * (businessPercentage + 0.05)).toInt }
    }
              
    if (linkConsumption.link.capacity.firstVal > 0) {
      val firstPercentage = (link.price.firstVal.toDouble / basePrice.firstVal.toDouble)
      if (firstLF < 60 && firstPercentage > 0.75) { newFirstPrice = (basePrice.firstVal * (firstPercentage - 0.05)).toInt }
      if (firstLF == 100) { newFirstPrice = (basePrice.firstVal * (firstPercentage + 0.05)).toInt }
    }

    if (significantChange(link.price.economyVal, newEconomyPrice) || significantChange(link.price.businessVal, newBusinessPrice) || significantChange(link.price.firstVal, newFirstPrice)) { 
      val newPrices = LinkClassValues(newEconomyPrice, newBusinessPrice, newFirstPrice)
      val newLink = link.copy(price = newPrices)
      //println(s"Updated link ${newLink.from.iata}-${newLink.to.iata} to new prices")
      newLinks += newLink
    }

    newLinks
  }

  def significantChange(original: Int, updated: Int): Boolean = {
    Math.abs(original - updated) > 2
  }

  case class FrequencyAdjustment(link: Link, assignedAirplanes: Map[Airplane, LinkAssignment])

  private def adjustFrequency(airline: Airline, flightLink: Link, linkConsumption: LinkConsumptionDetails, rivalFlightLinks: List[Link], linksByAirline: List[Link], totalLF: Int, basePrice: LinkClassValues, linksByAirlineAndAirportPair: Map[(Int,Int), List[Link]], cycle: Int): Option[FrequencyAdjustment] = {
    val demand = AISimUtil.getLinkDemand(flightLink.from, flightLink.to)
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
      
      if (AISimUtil.canBaseSupportLink(airline, flightLink.copy(frequency = newFrequency), linksByAirline, flightLink.from) && canAffordAirplane(airline, flightLink.getAssignedModel().get)) {
        val maxFrequencyPerAirplane = Computation.calculateMaxFrequency(flightLink.getAssignedModel().get, flightLink.distance)
        val airplanesRequired = Math.max(1, newFrequency / maxFrequencyPerAirplane)
        val assignedAirplanes = updateAssignedPlanes(flightLink.getAssignedModel().get, flightLink.airline, flightLink.from, newFrequency, flightLink.distance, airplanesRequired, maxFrequencyPerAirplane, cycle)
        val newLink = flightLink.copy(frequency = newFrequency)

        newLink.setAssignedAirplanes(assignedAirplanes)

        val negotiatedLink = AISimUtil.negotiateLink(airline, newLink, linksByAirlineAndAirportPair, cycle) match {
          case Some(value) =>
            println(s"Updating link ${newLink.from.iata}-${newLink.to.iata} with new increased frequency")
            
            Some(FrequencyAdjustment(newLink, assignedAirplanes))
            //LinkSource.updateLink(newLink)
            //LinkSource.updateAssignedPlanes(newLink.id, assignedAirplanes)
          case None =>
            None
            println(s"Negotiating increased frequency failed for link ${newLink.from.iata}-${newLink.to.iata}")
        }
      }
      None
    } else if (totalLF < 90 && flightLink.price.economyVal <= (basePrice.economyVal * 0.75)) {
      // decrease freq
      if (flightLink.frequency <= 3) {
        // try and downsize plane? or delete route
        println(s"Deleting route from ${flightLink.from.iata}-${flightLink.to.iata}")
        LinkSource.deleteLink(flightLink.id)
        None
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
        Some(FrequencyAdjustment(newLink, assignedAirplanes))
        //LinkSource.updateLink(newLink)
        //LinkSource.updateAssignedPlanes(newLink.id, assignedAirplanes)
      }
    }
    None
  }  

  /* 
  
    FLEET MANAGMENT
  
   */

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

  /* 
  
    BASE MANAGEMENT
  
   */
  
  private def baseManagement(airline: Airline, allAirports: List[Airport], linksByFromAirport: Map[Int, List[Link]], cycle: Int) = {
    println(s"Managing bases for ${airline.name}")
    
    val bases = airline.getBases()

    if (AISimUtil.allBasesAtCapacity(bases, airline, linksByFromAirport)) {
      // upgrade bases or expand to a new base
      val airlineCashFlow = CashFlowSource.loadCashFlowByAirline(airline.id, cycle, Period.WEEKLY)
      val cashFlow = if (airlineCashFlow.isDefined) airlineCashFlow.get.cashFlow else 0

      val baseToUpgrade = AISimUtil.getLowestScaleBase(bases)
      if (AISimUtil.canUpgradeBase(airline, baseToUpgrade, cashFlow)) {
        val upgradeCost = baseToUpgrade.calculateUpgradeCost(baseToUpgrade.scale + 1)
        AirlineSource.saveCashFlowItem(AirlineCashFlowItem(airline.id, CashFlowType.BASE_CONSTRUCTION, upgradeCost * -1))
        AirlineSource.saveAirlineBase(baseToUpgrade.copy(scale = (baseToUpgrade.scale + 1)))
        println(s"Base at ${baseToUpgrade.airport.iata} upgraded to scale ${baseToUpgrade.scale}")
      } else {
        // expand new base; shouldn't expand to new bases if less than 12 current bases
        if (bases.size < 12) {
          val newAirportBase = getNewExpansionBase(airline, AIAirlines.getAirlineStrategyProfile(airline.name), allAirports)
          if (newAirportBase.isDefined) {
            val base = AirlineBase(airline, newAirportBase.get, newAirportBase.get.countryCode, 1, cycle, false)
            val upgradeCost = base.calculateUpgradeCost(1)
            AirlineSource.saveCashFlowItem(AirlineCashFlowItem(airline.id, CashFlowType.BASE_CONSTRUCTION, upgradeCost * -1))
            AirlineSource.saveAirlineBase(base)
            println(s"New base opened at ${base.airport.iata}")
          }
        }
      }
    }
  }

  private def getNewExpansionBase(airline: Airline, airlineProfile: AIAirlines.AirlineStrategyProfile, allAirports: List[Airport]) : Option[Airport] = {
    val airportsFiltered = allAirports.filter { airport =>
      airport.population > 500000 &&
      (airlineProfile.primaryBaseCountries.contains(airport.countryCode) || airlineProfile.primaryBaseAffinities.contains(airport.zone))  
    }

    var newAirportBase : Option[Airport] = None
    var newAirportBaseScore = 0.0

    airportsFiltered.foreach { airport => 
      if (scoreAirportAsBase(airline, airport, airlineProfile) > newAirportBaseScore) {
        newAirportBase = Some(airport)
        newAirportBaseScore = scoreAirportAsBase(airline, airport, airlineProfile)
      }
    }

    newAirportBase
  }


  // decision tree logic

  private def scoreAirportAsBase(airline: Airline, airport: Airport, airlineProfile: AIAirlines.AirlineStrategyProfile) : Double = {
    val baseWeight = 100
    val populationScore = airport.population / 100_000_000.0
    val incomeScore = airport.income / 100_000.0
    val competitionScore = 1.0 - (AirportRating.rateAirport(airport).competitionRating.toDouble / 100)
    val relationshipScore = 1.0

    baseWeight * populationScore * incomeScore * competitionScore * relationshipScore
  }

  private def scoreNewLink(airline: Airline, airlineProfile: AIAirlines.AirlineStrategyProfile, fromAirport: Airport, toAirport: Airport, rivalLinks: List[Link]) : Double = {
    val demand = AISimUtil.getLinkDemand(fromAirport, toAirport)
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
}
