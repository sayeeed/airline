package com.patson.model

import FlightCategory._
import com.patson.data.AirportSource
import com.patson.model.airplane.Model
import com.patson.util.AirportCache

object AirlineBaseSpecialization extends Enumeration {
  abstract class Specialization() extends super.Val {
    val getType : BaseSpecializationType.Value
    val scaleRequirement : Int = 4
    val label : String
    def descriptions(airport : Airport) : List[String]
    val free = false
    def apply(airline : Airline, airport : Airport) {}
    def unapply(airline: Airline, airport : Airport) {}
  }

  case class DelegateSpecialization1() extends Specialization {
    override val getType = BaseSpecializationType.DELEGATE
    override val label = "Public Affairs I"
    override val scaleRequirement : Int = 12
    override def descriptions(airport : Airport) = List(s"Gives 1 extra delegate")
  }
  case class DelegateSpecialization2() extends Specialization {
    override val getType = BaseSpecializationType.DELEGATE
    override val label = "Public Affairs II"
    override val scaleRequirement : Int = 14
    override def descriptions(airport : Airport) = List(s"Gives 1 extra delegate")
  }

  case class PowerhouseSpecialization() extends Specialization {
    override val getType = BaseSpecializationType.AIRPORT_POWER
    override val label = "Powerhouse"
    override val scaleRequirement : Int = 14
    val populationBoost = 30000
    private[this] val minIncomeBoost = 3000 //should at least boost income by $3000
    private[this] val percentageBoost = 20 //20% if lower than minIncomeBoost then minIncomeBoost

    def incomeLevelBoost(airport : Airport) = {
      Computation.computeIncomeLevelBoostFromPercentage(airport.baseIncome, minIncomeBoost, percentageBoost)
    }
    override def descriptions(airport : Airport) =  {
      List(s"Increase population by $populationBoost", s"Increase income level by ${incomeLevelBoost(airport)}")
    }
    override def apply(airline: Airline, airport : Airport) = {
      AirportCache.invalidateAirport(airport.id)
    }
    override def unapply(airline: Airline, airport : Airport) = {
      AirportCache.invalidateAirport(airport.id)
    }
  }

  case class LoyaltySpecialization() extends Specialization {
    override val getType = BaseSpecializationType.LOYALTY
    override val label = "Local Sports Sponsorship"
    val loyaltyBoost = 10
    override val scaleRequirement : Int = 12
    override def descriptions(airport : Airport) = List(s"Boost loyalty of this airport by $loyaltyBoost")

    override def apply(airline: Airline, airport : Airport) = {
      unapply(airline, airport) //unapply first to avoid duplicates
      AirportSource.saveAirlineAppealBonus(airport.id, airline.id, AirlineBonus(BonusType.BASE_SPECIALIZATION_BONUS, AirlineAppeal(loyalty = loyaltyBoost), None))
    }

    override def unapply(airline: Airline, airport : Airport) = {
      AirportSource.loadAirlineAppealBonusByAirportAndAirline(airport.id, airline.id).find(_.bonusType == BonusType.BASE_SPECIALIZATION_BONUS).foreach { existingBonus =>
        AirportSource.deleteAirlineAppealBonus(airport.id, airline.id, BonusType.BASE_SPECIALIZATION_BONUS)
      }
    }
  }

  abstract class FlightTypeSpecialization extends Specialization {
    override val getType = BaseSpecializationType.FLIGHT_TYPE

    val staffModifier : (FlightCategory.Value, Model.Type.Value, Int) => Double
  }

  case class InternationalSpecialization() extends FlightTypeSpecialization {
    override val scaleRequirement : Int = 10
    override val staffModifier : ((FlightCategory.Value, Model.Type.Value, Int) => Double) = {
      case (INTERNATIONAL, _, _) => 0.9
      case _ => 1
    }
    override val label = "International Hub"
    override def descriptions(airport : Airport) = List("Reduce staff for international flights by 10%")
  }

  case class GateSmallSpecialization() extends FlightTypeSpecialization {
    override val staffModifier : ((FlightCategory.Value, Model.Type.Value, Int) => Double) = {
      case (_, Model.Type.SMALL, _) => 0.85
      case (_, Model.Type.PROPELLER_SMALL, _) => 0.85
      case _ => 1
    }
    override val label = "Apron Boarding Area"
    override def descriptions(airport : Airport) = List("Reduce staff for small aircraft flights by 15%")
  }

  case class GateRegionalSpecialization() extends FlightTypeSpecialization {
    override val staffModifier : ((FlightCategory.Value, Model.Type.Value, Int) => Double) = {
      case (_, Model.Type.REGIONAL, _) => 0.9
      case (_, Model.Type.PROPELLER_MEDIUM, _) => 0.9
      case _ => 1
    }
    override val label = "Commuter Gate Area"
    override def descriptions(airport : Airport) = List("Reduce staff for regional aircraft flights by 10%")
  }

  case class GateLargeSpecialization() extends FlightTypeSpecialization {
    override val scaleRequirement: Int = 10
    override val staffModifier : ((FlightCategory.Value, Model.Type.Value, Int) => Double) = {
      case (_, Model.Type.LARGE, _) => 0.9
      case (_, Model.Type.EXTRA_LARGE, _) => 0.9
      case (_, Model.Type.JUMBO, _) => 0.9
      case (_, Model.Type.JUMBO_XL, _) => 0.9
      case _ => 1.02
    }
    override val label = "Extra Large Gates"
    override def descriptions(airport : Airport) = List("Reduce staff for flights with widebody & jumbo aircraft by 10%", "Increase others by 2%")
  }

  case class GateMediumSpecialization() extends FlightTypeSpecialization {
    override val scaleRequirement : Int = 10
    override val staffModifier : ((FlightCategory.Value, Model.Type.Value, Int) => Double) = {
      case (_, Model.Type.MEDIUM, _) => 0.9
      case (_, Model.Type.MEDIUM_XL, _) => 0.9
      case _ => 1
    }
    override val label = "Uniform Ground Handling"
    override def descriptions(airport : Airport) = List("Reduce staff for flights with narrowbody aircraft by 10%")
  }

  case class GateSSTSpecialization() extends FlightTypeSpecialization {
    override val scaleRequirement : Int = 12
    override val staffModifier : ((FlightCategory.Value, Model.Type.Value, Int) => Double) = {
      case (_, Model.Type.SUPERSONIC, _) => 0.88
      case _ => 1
    }
    override val label = "SST Gate Area"
    override def descriptions(airport : Airport) = List("Reduce staff for flights with SSTs by 12%")
  }

  case class ServiceSpecialization1() extends FlightTypeSpecialization {
    override val scaleRequirement : Int = 12
    override val staffModifier : ((FlightCategory.Value, Model.Type.Value, Int) => Double) = {
      case (_, _, 1) => 0.88
      case _ => 1
    }
    override val label = "Fee Processing Center"
    override def descriptions(airport : Airport) = List("12% less staff on flights with 1 service star.")
  }

  case class ServiceSpecialization3() extends FlightTypeSpecialization {
    override val scaleRequirement : Int = 8
    override val staffModifier : ((FlightCategory.Value, Model.Type.Value, Int) => Double) = {
      case (_, _, 3) => 0.92
      case (_, _, 4) => 0.92
      case _ => 1
    }
    override val label = "Bulk Catering Facility"
    override def descriptions(airport : Airport) = List("8% less staff on flights with 3 or 4 service stars.")
  }

  case class ServiceSpecialization5() extends FlightTypeSpecialization {
    override val scaleRequirement : Int = 8
    override val staffModifier : ((FlightCategory.Value, Model.Type.Value, Int) => Double) = {
      case (_, _, 5) => 0.9
      case _ => 1
    }
    override val label = "Luxury Outfitter"
    override def descriptions(airport : Airport) = List("10% less staff on flights with 5 service star.")
  }


  sealed trait BrandSpecialization extends Specialization {
    override val getType = BaseSpecializationType.BRANDING
    val deltaByLinkClassAndPassengerType: Map[(LinkClass, PassengerType.Value), Double]
  }

  abstract class PassengerTypeBrandSpecialization extends BrandSpecialization {
    protected val passengerTypeDeltas: Map[PassengerType.Value, Double]

    override lazy val deltaByLinkClassAndPassengerType: Map[(LinkClass, PassengerType.Value), Double] = {
      val allLinkClasses = List(ECONOMY, BUSINESS, FIRST, DISCOUNT_ECONOMY)
      val allPassengerTypes = PassengerType.values.toList

      (for {
        lc <- allLinkClasses
        pt <- allPassengerTypes
      } yield {
        (lc, pt) -> passengerTypeDeltas.getOrElse(pt, 0.0)
      }).toMap
    }
  }

  abstract class LinkClassBrandSpecialization extends BrandSpecialization {
    protected val linkClassDeltas: Map[LinkClass, Double]

    override lazy val deltaByLinkClassAndPassengerType: Map[(LinkClass, PassengerType.Value), Double] = {
      val allLinkClasses = List(ECONOMY, BUSINESS, FIRST, DISCOUNT_ECONOMY)
      val allPassengerTypes = PassengerType.values.toList

      (for {
        lc <- allLinkClasses
        pt <- allPassengerTypes
      } yield {
        (lc, pt) -> linkClassDeltas.getOrElse(lc, 0.0)
      }).toMap
    }
  }

  // Example implementations:
  case class WifiSpecialization() extends PassengerTypeBrandSpecialization {
    override val scaleRequirement: Int = 4
    override val label = "WiFi & Outlets"
    override def descriptions(airport: Airport) = List("Adds a 3% preference for travelers", "Adds a 6% preference for business people")
    override protected val passengerTypeDeltas = Map(
      PassengerType.BUSINESS -> -0.04,
      PassengerType.TRAVELER -> -0.02
    )
  }

  case class BudgetAirlineSpecialization() extends LinkClassBrandSpecialization {
    override val scaleRequirement: Int = 8
    override val label = "Cheap Fast Food Restaurants"
    override def descriptions(airport: Airport) = List("Adds an 5% preference for discount economy pax", "Adds a 2% preference for economy pax")
    override protected val linkClassDeltas = Map(
      DISCOUNT_ECONOMY -> -0.05,
      ECONOMY -> -0.02
    )
  }

  case class PremiumAirlineSpecialization() extends LinkClassBrandSpecialization {
    override val scaleRequirement: Int = 12
    override val label = "Luxury Shops & Dining"
    override def descriptions(airport: Airport) = List("Adds an 4% preference for first class pax", "Adds a 2% preference for business class pax")
    override protected val linkClassDeltas = Map(
      BUSINESS -> -0.02,
      FIRST -> -0.04
    )
  }

  case class HelpSpecialization() extends PassengerTypeBrandSpecialization {
    override val scaleRequirement : Int = 4
    override val label = "Help Desks"
    override def descriptions(airport : Airport) = List("Adds a 2% preference for travelers", "Adds a 4% preference for tourists")
    override protected val passengerTypeDeltas = Map(
      PassengerType.TOURIST -> -0.04,
      PassengerType.TRAVELER -> -0.02,
    )
  }
  case class VIPSpecialization1() extends PassengerTypeBrandSpecialization {
    override val scaleRequirement : Int = 4
    override val label = "VIP Chauffeurs"
    override def descriptions(airport : Airport) = List("Adds a 5% preference for elites")
    override protected val passengerTypeDeltas = Map(
      PassengerType.ELITE -> -0.05,
    )
  }
  case class SecuritySpecialization() extends PassengerTypeBrandSpecialization {
    override val scaleRequirement : Int = 4
    override val label = "Fast Track Security"
    override def descriptions(airport : Airport) = List("Adds a 4% preference for business people")
    override protected val passengerTypeDeltas = Map(
      PassengerType.BUSINESS -> -0.04,
    )
  }
  case class PrioritySpecialization() extends LinkClassBrandSpecialization {
    override val scaleRequirement : Int = 8
    override val label = "Priority Check-In"
    override def descriptions(airport : Airport) = List("Adds a 5% preference for first & business class")
    override protected val linkClassDeltas = Map(
      BUSINESS -> -0.04,
      FIRST -> -0.04
    )
  }
  case class KidsSpecialization() extends PassengerTypeBrandSpecialization {
    override val scaleRequirement : Int = 8
    override val label = "Kids' Playzones"
    override def descriptions(airport : Airport) = List("Adds a 3% preference for travelers", "Adds a 6% preference for tourists")
    override protected val passengerTypeDeltas = Map(
      PassengerType.TOURIST -> -0.04,
      PassengerType.TRAVELER -> -0.02,
    )
  }
  case class RatsSpecialization() extends LinkClassBrandSpecialization {
    override val scaleRequirement : Int = 10
    override val label = "Public Rat Cuddling Lounge"
    override def descriptions(airport : Airport) = List("Adds a 2% preference for all but first class")
    override protected val linkClassDeltas = Map(
      DISCOUNT_ECONOMY -> -0.02,
      ECONOMY -> -0.02,
      BUSINESS -> -0.02,
    )
  }
  case class VIPSpecialization2() extends PassengerTypeBrandSpecialization {
    override val scaleRequirement : Int = 8
    override val label = "VIP Suites"
    override def descriptions(airport : Airport) = List("Adds a 3% preference for travelers", "Adds a 6% preference for tourists")
    override protected val passengerTypeDeltas = Map(
      PassengerType.ELITE -> -0.04
    )
  }
  case class OlympicSpecialization() extends PassengerTypeBrandSpecialization {
    override val scaleRequirement : Int = 10
    override val label = "Airport Olympic Expo"
    override def descriptions(airport : Airport) = List("Adds a 10% preference for olympic passengers")
    override protected val passengerTypeDeltas = Map(
      PassengerType.OLYMPICS -> -0.1
    )
  }

  case class HangarSpecialization1() extends Specialization {
    override val getType = BaseSpecializationType.HANGAR
    override val label = "Maintenance Hangar"
    override val scaleRequirement : Int = 10
    override def descriptions(airport : Airport) = List(s"5% reduction in delays / cancellations for each hangar on a link.")
  }
  case class HangarSpecialization2() extends Specialization {
    override val getType = BaseSpecializationType.HANGAR
    override val label = "Maintenance Hangar II"
    override val scaleRequirement : Int = 14
    override def descriptions(airport : Airport) = List(s"5% reduction in delays / cancellations for each hangar on a link.")
  }

  implicit def valueToSpecialization(x: Value) = x.asInstanceOf[Specialization]

  val INTERNATIONAL_HUB = InternationalSpecialization()
  val GATE_SMALL = GateSmallSpecialization()
  val GATE_REGIONAL = GateRegionalSpecialization()
  val GATE_SST = GateSSTSpecialization()
  val GATE_WIDE = GateLargeSpecialization()
  val GATE_MEDIUM = GateMediumSpecialization()
  val SERVICE_1 = ServiceSpecialization1()
  val SERVICE_3 = ServiceSpecialization3()
  val SERVICE_5 = ServiceSpecialization5()
  val SPORTS_SPONSORSHIP = LoyaltySpecialization()
  val DELEGATE_RECRUITER_1 = DelegateSpecialization1()
  val DELEGATE_RECRUITER_2 = DelegateSpecialization2()
  val BRANDING_BUDGET = BudgetAirlineSpecialization()
  val BRANDING_PREMIUM = PremiumAirlineSpecialization()
  val BRANDING_WIFI = WifiSpecialization()
  val BRANDING_HELP = HelpSpecialization()
  val BRANDING_SECURITY = SecuritySpecialization()
  val BRANDING_PRIORITY = PrioritySpecialization()
  val BRANDING_KIDS = KidsSpecialization()
  val BRANDING_RATS = RatsSpecialization()
  val BRANDING_VIP_1 = VIPSpecialization1()
  val BRANDING_VIP_2 = VIPSpecialization2()
  val BRANDING_OLYMPIC = OlympicSpecialization()
  val POWERHOUSE = PowerhouseSpecialization()
  val HANGAR_1 = HangarSpecialization1()
  val HANGAR_2 = HangarSpecialization2()
}

object BaseSpecializationType extends Enumeration {
  type SpecializationType = Value
  val FLIGHT_TYPE, DELEGATE, HANGAR, BRANDING, LOYALTY, AIRPORT_POWER = Value
  val COOLDOWN = 12 //change every 100 cycles
}