package controllers

import java.util.Random

import com.patson.data.{AirlineSource, AirplaneSource, AirportSource, BankSource, CycleSource, TutorialSource}
import com.patson.model._
import com.patson.model.airplane._
import com.patson.util.AirportCache
import controllers.AuthenticationObject.AuthenticatedAirline
import javax.inject.Inject
import models.Profile
import play.api.libs.json.{JsValue, Json, _}
import play.api.mvc._

import scala.collection.mutable.ListBuffer

class ProfileApplication @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
  implicit object ProfileWrites extends Writes[Profile] {
    def writes(profile: Profile): JsValue = {
      var result =Json.obj(
        "name" -> profile.name,
        "type" -> profile.airlineType,
        "difficulty" -> profile.difficulty,
        "description" -> profile.description,
        "rule" -> profile.rule,
        "cash" -> profile.cash,
        "quality" -> profile.quality,
        "airplanes" -> profile.airplanes,
        "reputation" -> profile.reputation,
        "airportText" -> profile.airport.displayText)
      profile.loan.foreach { loan =>
        result = result + ("loan" -> Json.toJson(loan)(new LoanWrites(CycleSource.loadCycle())))
      }
      result
    }
  }

  val BASE_CAPITAL = 60_000_000
  val BONUS_PER_DIFFICULTY_POINT = 1350000

  def generateAirplanes(value : Int, capacityRange : scala.collection.immutable.Range, quality : Int, homeAirport : Airport, condition : Double, airline : Airline, random : Random) : List[Airplane] =  {
    val qualityRange = (quality - 2) to (quality + 2)
    val eligibleModels = allAirplaneModels.filter(model => capacityRange.contains(model.capacity))
      .filter(model => model.purchasableWithRelationship(allCountryRelationships.getOrElse((homeAirport.countryCode, model.countryCode), 0)))
      .filter(model => model.price * condition / Airplane.MAX_CONDITION <= value / 3)
      .filter(model => model.runwayRequirement <= homeAirport.runwayLength)
      .filter(model => model.range >= 1400)
      .filter(model => qualityRange.contains(model.quality))
    val countryModels = eligibleModels.filter(_.countryCode == homeAirport.countryCode)
    val currentCycle = CycleSource.loadCycle()

    val selectedModel =
      if (eligibleModels.isEmpty) {
        None
      } else {
        if (countryModels.nonEmpty) { //always go for airplanes from this country first
          Some(countryModels(random.nextInt(countryModels.length)))
        } else {
          Some(eligibleModels(random.nextInt(eligibleModels.length)))
        }
      }
    selectedModel match {
      case Some(model) =>
        val amount = Math.min(value / model.price, 7)
        val age = (Airplane.MAX_CONDITION - condition) / (Airplane.MAX_CONDITION.toDouble / model.lifespan)  //not really that useful, just to fake a more reasonable number
        val constructedCycle = Math.max(0, currentCycle - age.toInt)
        (0 until amount).map(_ => Airplane(model, airline, constructedCycle, constructedCycle, condition, depreciationRate = 0, value = (model.price * condition / Airplane.MAX_CONDITION).toInt, home = homeAirport)).toList
      case None =>
        List.empty
    }
  }

  val BASE_INTEREST_RATE = 0.08

  def generateProfiles(airline : Airline, airport : Airport) : List[Profile] = {
    val difficulty = airport.rating.overallDifficulty
    val capital = BASE_CAPITAL + difficulty * BONUS_PER_DIFFICULTY_POINT

    val profiles = ListBuffer[Profile]()

    val random = new Random(airport.id)



    val smallAirplanes = generateAirplanes(capital, (10 to 90), 2, airport, 90, airline, random)
    if (smallAirplanes.nonEmpty) {
      val beginnerProfile = Profile(
        name = "Beginner's start",
        airlineType = AirlineType.BEGINNER,
        difficulty = "Easy",
        description = "Recommended for new players. This is for learning the game!",
        rule = List("You will never go above 250 reputation!", "Base crew costs are 25% lower & service costs are 30% lower."),
        airplanes = smallAirplanes,
        reputation = 20,
        cash = capital * 2,
        airport = airport,
        loan = Some(Bank.getLoanOptions((capital).toInt, BASE_INTEREST_RATE / 2, CycleSource.loadCycle()).last.copy(airlineId = airline.id))
      )
      profiles.append(beginnerProfile)
    }

    val loanProfile = Profile(
      name = "Entrepreneurial spirit",
      airlineType = AirlineType.LEGACY,
      description = "You and the bank are betting big that there's money in commercial aviation! Plan carefully but make bold moves to thrive in this brave new world!",
      cash = (capital * 2.5).toInt,
      airport = airport,
      loan = Some(Bank.getLoanOptions((capital * 2).toInt, BASE_INTEREST_RATE, CycleSource.loadCycle()).last.copy(airlineId = airline.id))
    )
    profiles.append(loanProfile)

    val largeAirplanes = generateAirplanes((capital * 4).toInt, (airport.size * 12 to airport.size * 20), 5, airport, 72, airline, random)
    if (largeAirplanes.nonEmpty) {
      val largeAirplaneProfile = Profile(
        name = "Revival of past glory",
        airlineType = AirlineType.LEGACY,
        description = "A once great airline now saddled with debt and aging airplanes. Can you turn this airline around?",
        cash = (capital * 4.6).toInt - largeAirplanes.map(_.value).sum,
        airport = airport,
        reputation = 30,
        quality = 0,
        airplanes = largeAirplanes,
        loan = Some(Bank.getLoanOptions((capital * 4).toInt, BASE_INTEREST_RATE, CycleSource.loadCycle()).last.copy(airlineId = airline.id))
      )
      profiles.append(largeAirplaneProfile)
    }

    val megaHQ = Profile(
      name = "Mega HQ",
      airlineType = AirlineType.MEGA_HQ,
      description = "Your home town has given you low interest bonds to connect it to the world!",
      rule = List("Upgrading & upkeep your HQ base is much cheaper", "Upgrading & upkeep on all other bases is more expensive"),
      cash = (capital * 3).toInt + difficulty * BONUS_PER_DIFFICULTY_POINT, //receive double bonus for starting in small airport
      airport = airport,
      loan = Some(Bank.getLoanOptions((capital * 2).toInt, BASE_INTEREST_RATE / 2, CycleSource.loadCycle()).last.copy(airlineId = airline.id))
    )
    profiles.append(megaHQ)

    val ULCCAirplanes = generateAirplanes((capital * 2.75).toInt, (airport.size * 14 to airport.size * 22), 2, airport, 85, airline, random)
    if (!ULCCAirplanes.isEmpty) {
      val cheapAirplaneProfile = Profile(
        name = "ULCC Airline",
        airlineType = AirlineType.ULCC,
        difficulty = "Hard",
        description = "Time to pack in the masses!",
        rule = List("You can never add business or first class!","2x reputation from tourist track","Base crew costs are 25% lower"),
        cash = (capital * 3.25).toInt - ULCCAirplanes.map(_.value).sum,
        airport = airport,
        reputation = 20,
        quality = 0,
        airplanes = ULCCAirplanes,
        loan = Some(Bank.getLoanOptions((capital * 2.5).toInt, BASE_INTEREST_RATE, CycleSource.loadCycle()).last.copy(airlineId = airline.id)))
      profiles.append(cheapAirplaneProfile)
    }

    val fancyAirplanes = generateAirplanes((capital * 2.5).toInt, (36 to 90), 9, airport, 85, airline, random)
    if (!fancyAirplanes.isEmpty) {
      val fancyAirplaneProfile = Profile(
        name = "Luxury Startup",
        airlineType = AirlineType.LUXURY,
        difficulty = "Hard",
        description = "A highly motivated team with high quality aircraft. Perfect for premium service!",
        rule = List("You can never add an economy class!","2x reputation from elite track","Service quality costs are 30% lower"),
        cash = (capital * 3.25).toInt - fancyAirplanes.map(_.value).sum,
        airport = airport,
        reputation = 25,
        quality = 90,
        airplanes = fancyAirplanes,
        loan = Some(Bank.getLoanOptions((capital * 2).toInt, BASE_INTEREST_RATE, CycleSource.loadCycle()).last.copy(airlineId = airline.id)))
      profiles.append(fancyAirplaneProfile)
    }

    profiles.toList
  }

  def getProfiles(airlineId : Int, airportId : Int) = AuthenticatedAirline(airlineId) { request =>
    request.user.getHeadQuarter() match {
      case Some(headquarters) =>
        BadRequest("Cannot select profile with active HQ")
      case None =>
        Ok(Json.toJson(generateProfiles(request.user, AirportCache.getAirport(airportId, true).get)))
    }

  }

  private[this] val buildHqWithProfileLock = new Object()
  def buildHqWithProfile(airlineId : Int, airportId : Int, profileId : Int) = AuthenticatedAirline(airlineId) { request =>
    val airline = request.user
    buildHqWithProfileLock.synchronized {
      if (!airline.isInitialized) {
        val airport = AirportCache.getAirport(airportId, true).get
        val profile = generateProfiles(airline, airport)(profileId)
        val targetQuality = Math.max(35, profile.quality)

        val base = AirlineBase(airline, airport, airport.countryCode, 1, CycleSource.loadCycle(), true)
        airline.airlineType = profile.airlineType
        AirlineSource.updateAirlineType(airlineId, airline.airlineType.id)
        AirlineSource.saveAirlineBase(base)
        airline.setCountryCode(airport.countryCode)
        airline.setReputation(profile.reputation)
        airline.setCurrentServiceQuality(profile.quality)
        airline.setTargetServiceQuality(targetQuality)
        airline.setBalance(profile.cash)
        AirportSource.updateAirlineAppeal(airportId, airlineId, AirlineAppeal(loyalty = 0))

        profile.airplanes.foreach(_.assignDefaultConfiguration())
        AirplaneSource.saveAirplanes(profile.airplanes)

        profile.loan.foreach { loan =>
          BankSource.saveLoan(loan)
        }

        airline.setInitialized(true)
        AirlineSource.saveAirlineInfo(airline, true)
        val updatedAirline = AirlineSource.loadAirlineById(airlineId, true)

        Ok(Json.toJson(updatedAirline))
      } else {
        BadRequest(s"${request.user} was already initialized")
      }
    }
  }
}
