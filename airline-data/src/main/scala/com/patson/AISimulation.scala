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
      
      aiAirlines.foreach { airline =>
        if (Random.nextDouble() < 1) {
          val airlineFlightLinks = flightLinksByAirline.getOrElse(airline.id, Nil)
          val rivalFlightLinks = allFlightLinks.filterNot(_.id == airline.id)
          val linksByFromAirport = flightLinksByAirline.get(airline.id).getOrElse(List.empty).groupBy(_.from.id)
            
          routeManagement(airline, airlineFlightLinks, flightLinksByAirline, cycle)
          baseManagement(airline, allAirports, linksByFromAirport, cycle)
          addNewRoutes(airline, AIAirlines.getAirlineStrategyProfile(airline.name), allAirports, rivalFlightLinks, linksByFromAirport, cycle)
        }
      }
    } 
  }

  /* 
  
    ROUTE MANAGMENT

   */

  private def routeManagement(airline: Airline, flightLinks: List[Link], flightLinksByAirline: Map[Int, List[Link]], cycle: Int) = {
    println(s"Updated routes for ${airline.name}")
    
    flightLinks.foreach {
      case flightLink : Link =>
        val linkConsumptions = LinkSource.loadLinkConsumptionsByLinkId(flightLink.id, cycle).headOption
        val rivalFlightLinks = LinkSource.loadFlightLinksByAirports(flightLink.from.id, flightLink.to.id).filterNot(_.airline.id == flightLink.airline.id)

        linkConsumptions.foreach { linkConsumption =>
          val basePrice = LinkClassValues(linkConsumption.link.price.economyVal, linkConsumption.link.price.businessVal, linkConsumption.link.price.firstVal)
            
          // all 10-week averages
          val (economyLF, businessLF, firstLF, totalLF) = AISimUtil.computeAverageLoadFactor(flightLink, cycle)
          val (avgEconomyPrice, avgBusinessPrice, avgFirstPrice) = AISimUtil.getAverageRivalPrice(flightLink, rivalFlightLinks)

          // adjusts prices down if total load factor is below 90% and no major delays or cancellations occurred
          adjustPrices(flightLink, linkConsumption, basePrice, economyLF, businessLF, firstLF)
          adjustFrequency(airline, flightLink, linkConsumption, rivalFlightLinks, flightLinksByAirline, totalLF, basePrice, cycle)
        }
    }
  }

  private def addNewRoutes(airline: Airline, airlineProfile: AIAirlines.AirlineStrategyProfile, allAirports: List[Airport], rivalLinks: List[Link], linksByFromAirport: Map[Int, List[Link]], cycle: Int) = {
    println(s"Adding new routes for ${airline.name}")
    
    val bases = airline.getBases()
    val base = AISimUtil.getLowestCapacityBase(bases, airline, linksByFromAirport)
    val models = allModels.filter(model => airlineProfile.modelNames.contains(model.name))
    val nearbyAirports = allAirports.filter(airport => airlineProfile.preferredCountries.contains(airport.countryCode) && airport.population > 500_000 && airport != base.airport)
    val farAirports = allAirports.filter(airport => airport.zone.split("\\|").map(_.trim).exists(airlineProfile.preferredAffinities.contains) && airport.population > 1_000_000 && airport.isGateway() && !nearbyAirports.contains(airport) & airport != base.airport)

    // TODO: implement delegates when placing a new route or upgrading frequency
    var attempts = 0
    while (airline.getDelegateInfo().availableCount > 1 && airline.airlineInfo.balance > 100_000_000 && attempts < 20) {
      if (Random.nextDouble() < 0.8) {
        generateLinks(
          LinkGeneration(
            fromAirport = base.airport,
            toAirports = nearbyAirports,
            models = models,
            airline = airline,
            config = LinkConfig("near", nearbyAirports.size, 1, airlineProfile.routeServiceLevel)
          ), airline, rivalLinks, cycle
        )
      } else {
        generateLinks(
          LinkGeneration(
            fromAirport = base.airport,
            toAirports = farAirports,
            models = models,
            airline = airline,
            config = LinkConfig("far", farAirports.size, 1, Math.min(100, airlineProfile.routeServiceLevel + 20))
          ), airline, rivalLinks, cycle
        )
      }
      attempts += 1
    }
  }

  private def generateLinks(config: LinkGeneration, airline: Airline, rivalLinks: List[Link], cycle: Int) : List[Link] = {
    val airlineProfile = AIAirlines.getAirlineStrategyProfile(airline.name)
    val airplaneModelsLarge = config.models.sortBy(_.capacity).reverse
    val airplaneModelsSmall = config.models.sortBy(_.capacity)

    val scoredRoutes = config.toAirports
      .filter(_.id != config.fromAirport.id)
      .filterNot(link => LinkSource.loadFlightLinkByAirportsAndAirline(config.fromAirport.id, link.id, airline.id).nonEmpty)
      .flatMap { toAirport =>
        val demand = AISimUtil.getLinkDemand(config.fromAirport, toAirport)
        val distance = Computation.calculateDistance(config.fromAirport, toAirport)
        val targetSeats = (demand.travelerDemand.total + demand.businessDemand.total) * 2
        val linkCost = Computation.getLinkCreationCost(config.fromAirport, toAirport)

        if (!AISimUtil.isLinkSaturated(airline, config.fromAirport, toAirport) && targetSeats > 0 && (demand.businessDemand.total + demand.touristDemand.total + demand.travelerDemand.total) > airlineProfile.minimumRouteDemand && linkCost < airline.airlineInfo.balance) {
          val score = scoreNewLink(airline, airlineProfile, config.fromAirport, toAirport, rivalLinks)
          Some(ScoredRoute(toAirport, score, distance, targetSeats))
        } else {
          None
        }
      }

    val topRoutes = scoredRoutes.sortBy(-_.score).take(config.config.linkCount)
    val newLinks = topRoutes.flatMap { route => 
      //println(s"Creating a new route ${config.fromAirport.iata}-${route.toAirport.iata}")
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

      if (frequency > 0 && canAffordAirplane(airline, pickedModel.get)) {
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

        val econPrice = (Pricing.computeStandardPrice(distance, Computation.getFlightCategory(fromAirport, toAirport), ECONOMY, PassengerType.BUSINESS, fromAirport.baseIncome)).toInt
        val bizPrice = (Pricing.computeStandardPrice(distance, Computation.getFlightCategory(fromAirport, toAirport), BUSINESS, PassengerType.BUSINESS, fromAirport.baseIncome)).toInt
        val firstPrice = (Pricing.computeStandardPrice(distance, Computation.getFlightCategory(fromAirport, toAirport), FIRST, PassengerType.BUSINESS, fromAirport.baseIncome)).toInt

        val duration = Computation.calculateDuration(model, distance)
        val capacity = AISimUtil.calculateTotalCapacity(assignedAirplanes)

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

        val existingLink : Option[Link] = LinkSource.loadFlightLinkByAirportsAndAirline(fromAirport.id, toAirport.id, airline.id)
        val negotiationInfo = NegotiationUtil.getLinkNegotiationInfo(airline, link, existingLink)
        val delegateCount = Math.ceil(negotiationInfo.finalRequirementValue).toInt
        println(s"1 delegate count required: ${delegateCount}")
        println(s"2 delegates available now: ${airline.getDelegateInfo().availableCount}")
        if (delegateCount <= airline.getDelegateInfo().availableCount && delegateCount <= NegotiationUtil.MAX_ASSIGNED_DELEGATE) {
          println("3 Starting negotiation")
          val negotiationResultOption =
            if(negotiationInfo.finalRequirementValue > 0) {
              Some(NegotiationUtil.negotiate(negotiationInfo, delegateCount))
            } else {
              None
            }

          if (negotiationResultOption.map(_.isSuccessful).getOrElse(true)) {
            println(s"4 ${airline.name} added a new link (${link.from.iata}-${link.to.iata}) with ${capacity} total capacity.")

            val linkCost = Computation.getLinkCreationCost(fromAirport, toAirport)
            AirlineSource.saveCashFlowItem(AirlineCashFlowItem(airline.id, CashFlowType.CREATE_LINK, linkCost * -1))
            AirlineSource.adjustAirlineBalance(airline.id, linkCost * -1)

            Some(link)
          }

          negotiationResultOption.foreach { negotiationResult =>
            println(s"4.5 Negotiation successful?: ${negotiationResult.isSuccessful}")
            println(s"5 Delegates available before negotiation: ${airline.getDelegateInfo().availableCount}")
            
            //update delegate status
            val cycle = CycleSource.loadCycle()
            val task = DelegateTask.linkNegotiation(cycle, fromAirport, toAirport)
            val coolDown = if (negotiationResult.isSuccessful) task.coolDown else task.coolDown / 2 //half cooldown if it was unsuccessful
            val availableCycle = cycle + coolDown
      
            val busyDelegates = (0 until delegateCount).toList.map { _ =>
              BusyDelegate(airline, task, Some(availableCycle))
            }
      
            DelegateSource.saveBusyDelegates(busyDelegates)
      
            LinkSource.saveNegotiationCoolDown(airline, link.from, link.to, cycle + Link.LINK_NEGOTIATION_COOL_DOWN)

            println(s"6 Delegates available after negotiation: ${airline.getDelegateInfo().availableCount}")
          }
          None
        }
        None
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
      //println(s"Updated link ${newLink.from.iata}-${newLink.to.iata} to new prices")
      LinkSource.updateLink(newLink)
    }
  }

  private def adjustFrequency(airline: Airline, flightLink: Link, linkConsumption: LinkConsumptionDetails, rivalFlightLinks: List[Link], flightLinksByAirline: Map[Int,List[Link]], totalLF: Int, basePrice: LinkClassValues, cycle: Int) = {
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
      
      if (AISimUtil.canBaseSupportLink(airline, flightLink.copy(frequency = newFrequency), flightLinksByAirline, flightLink.from) && canAffordAirplane(airline, flightLink.getAssignedModel().get)) {
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
          val base = AirlineBase(airline, newAirportBase, newAirportBase.countryCode, 1, cycle, false)
          val upgradeCost = base.calculateUpgradeCost(1)
          AirlineSource.saveCashFlowItem(AirlineCashFlowItem(airline.id, CashFlowType.BASE_CONSTRUCTION, upgradeCost * -1))
          AirlineSource.saveAirlineBase(base)
          println(s"New base opened at ${base.airport.iata}")
        }
      }
    }
  }

  private def getNewExpansionBase(airline: Airline, airlineProfile: AIAirlines.AirlineStrategyProfile, allAirports: List[Airport]) : Airport = {
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


  // decision tree logic

  private def scoreAirportAsBase(airline: Airline, airport: Airport, airlineProfile: AIAirlines.AirlineStrategyProfile) : Double = {
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
