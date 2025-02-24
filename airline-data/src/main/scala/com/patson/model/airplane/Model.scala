package com.patson.model.airplane

import com.patson.model.IdObject
import com.patson.model.Airline
import com.patson.model.airplane.Model.Category
import com.patson.util.AirplaneModelCache

/**
 *
 * @param name
 * @param family
 * @param capacity
 * @param quality 0-10 quality "passenger experience"
 * @param ascentBurn
 * @param cruiseBurn
 * @param speed
 * @param range
 * @param price
 * @param lifespan
 * @param constructionTime
 * @param manufacturer
 * @param runwayRequirement
 * @param imageUrl
 * @param id
 */
case class Model(name : String, family : String = "", capacity : Int, quality : Int, ascentBurn : Double, cruiseBurn : Double, speed : Int, range : Int, price : Int, lifespan : Int, constructionTime : Int, manufacturer: Manufacturer, runwayRequirement : Int, imageUrl : String = "", var id : Int = 0) extends IdObject {
  import Model.Type._

  val countryCode = manufacturer.countryCode
  val SUPERSONIC_SPEED_THRESHOLD = 1236
  val SIZE_SMALL_THRESHOLD = 80

  val airplaneType : Type = {
    if (speed > SUPERSONIC_SPEED_THRESHOLD) {
      SUPERSONIC
    } else if (speed < 180) {
      AIRSHIP
    } else if (speed <= 300) {
      HELICOPTER
    } else if (speed <= 680) {
      if (capacity <= SIZE_SMALL_THRESHOLD) {
        PROPELLER_SMALL
      } else {
        PROPELLER_MEDIUM
      }
    } else if (capacity <= SIZE_SMALL_THRESHOLD) {
      SMALL
    } else if (capacity <= 131 || family == "Embraer E-Jet E2") {
      REGIONAL
    } else if (capacity <= 215) {
      MEDIUM
    } else if (family == "Boeing 757" || family == "Boeing 737"  || family == "Airbus A320" || family == "DC-8" || family == "Yakovlev MC-21") {
      MEDIUM_XL
    } else if (capacity <= 375) {
      LARGE
    } else if (capacity <= 475) {
      EXTRA_LARGE
    } else if (capacity <= 800) {
      JUMBO
    } else {
      JUMBO_XL
    }
  }

  val category: Model.Category.Value = Category.fromType(airplaneType)

  private[this]val BASE_TURNAROUND_TIME = 35
  val turnaroundTime : Int = {
    BASE_TURNAROUND_TIME +
      (airplaneType match {
        case HELICOPTER => 0 //buffing for short distances
        case AIRSHIP => 0
        case _ => capacity / 3.75
      }).toInt
  }

  val airplaneTypeLabel : String = label(airplaneType)

  val airplaneTypeSize : Double = size(airplaneType)

  //weekly fixed cost
  val baseMaintenanceCost : Int = {
    (capacity / 50.0 * 725).toInt //46,500
    (capacity * 155).toInt //1240, 37820
  }

  def applyDiscount(discounts : List[ModelDiscount]) = {
    var discountedModel = this
    discounts.groupBy(_.discountType).foreach {
      case (discountType, discounts) => discountType match {
        case DiscountType.PRICE =>
          val totalDiscount = discounts.map(_.discount).sum
          discountedModel = discountedModel.copy(price = (price * (1 - totalDiscount)).toInt)
        case DiscountType.CONSTRUCTION_TIME =>
          var totalDiscount = discounts.map(_.discount).sum
          totalDiscount = Math.min(1, totalDiscount)
          discountedModel = discountedModel.copy(constructionTime = (constructionTime * (1 - totalDiscount)).toInt)
      }
    }
    discountedModel
  }

  val purchasableWithRelationship = (relationship : Int) => {
    relationship >= Model.BUY_RELATIONSHIP_THRESHOLD
  }
}

object Model {
  val BUY_RELATIONSHIP_THRESHOLD = 0

  def fromId(id : Int) = {
    val modelWithJustId = Model("Unknown", "Unknown", 0, 0, 0, 0, 0, 0, 0, 0, 0, Manufacturer("Unknown", countryCode = ""), runwayRequirement = 0)
    modelWithJustId.id = id
    modelWithJustId
  }

  object Type extends Enumeration {
    type Type = Value
    val AIRSHIP, HELICOPTER, PROPELLER_SMALL, PROPELLER_MEDIUM, SMALL, REGIONAL, MEDIUM, MEDIUM_XL, LARGE, EXTRA_LARGE, JUMBO, JUMBO_XL, SUPERSONIC = Value

    val label: Type => String = {
      case AIRSHIP => "Airship"
      case HELICOPTER => "Helicopter"
      case PROPELLER_SMALL => "Small Prop"
      case PROPELLER_MEDIUM => "Regional Prop"
      case SMALL => "Small Jet"
      case REGIONAL => "Regional Jet"
      case MEDIUM => "Narrow-body"
      case MEDIUM_XL => "Narrow-body XL"
      case LARGE => "Wide-body"
      case EXTRA_LARGE => "Wide-body XL"
      case JUMBO => "Jumbo Jet"
      case JUMBO_XL => "Jumbo XL"
      case SUPERSONIC => "Supersonic"
    }

    val size: Type => Double = {
      case HELICOPTER => 0.03
      case PROPELLER_SMALL => 0.055
      case PROPELLER_MEDIUM => 0.095
      case SMALL => 0.055
      case REGIONAL => 0.095
      case MEDIUM => 0.14
      case MEDIUM_XL => 0.18
      case AIRSHIP => 0.2
      case SUPERSONIC => 0.24
      case LARGE => 0.20
      case EXTRA_LARGE => 0.25
      case JUMBO => 0.3
      case JUMBO_XL => 0.33
    }
  }

  object Category extends Enumeration {
    type Category = Value
    val SPECIAL, SMALL, REGIONAL, MEDIUM, LARGE, EXTRAORDINARY = Value
    val grouping: Map[Model.Category.Value, List[Type.Value]] = Map(
      SMALL -> List(Type.SMALL, Type.PROPELLER_SMALL),
      REGIONAL -> List(Type.REGIONAL, Type.PROPELLER_MEDIUM),
      MEDIUM -> List(Type.MEDIUM, Type.MEDIUM_XL),
      LARGE -> List(Type.LARGE, Type.EXTRA_LARGE),
      EXTRAORDINARY -> List(Type.JUMBO, Type.JUMBO_XL, Type.SUPERSONIC),
      SPECIAL -> List(Type.AIRSHIP, Type.HELICOPTER),
    )

    val fromType: Type.Value => Model.Category.Value = (airplaneType : Type.Value) => {
      grouping.find(_._2.contains(airplaneType)).map(_._1).getOrElse(Model.Category.EXTRAORDINARY)
    }

    val capacityRange : Map[Category.Value, (Int, Int)]= {
      AirplaneModelCache.allModels.values.groupBy(_.category).view.mapValues { models =>
        val sortedByCapacity = models.toList.sortBy(_.capacity)
        (sortedByCapacity.head.capacity, sortedByCapacity.last.capacity)
      }.toMap
    }

    val speedRange: Map[Category.Value, (Int, Int)] = {
      AirplaneModelCache.allModels.values.groupBy(_.category).view.mapValues { models =>
        val sortedBySpeed = models.toList.sortBy(_.speed)
        (sortedBySpeed.head.speed, sortedBySpeed.last.speed)
      }.toMap
    }

    def getCapacityRange(category: Category.Value): (Int, Int) = {
      capacityRange.getOrElse(category, (0, 0))
    }

    def getSpeedRange(category: Category.Value): (Int, Int) = {
      speedRange.getOrElse(category, (0, 0))
    }

  }

  val models = List(
Model("Airbus A220-300",	"Airbus A220",	160,	8,	0.95,	1.28,	828,	4745,	129914506,	1820,	14,	Manufacturer("Airbus",	"NL"),	1890,	"https://www.norebbo.com/2016/02/bombardier-cs300-blank-illustration-templates/"),
Model("Airbus A221",	"Airbus A220",	188,	8,	0.98,	1.16,	828,	4800,	169161620,	1820,	20,	Manufacturer("Airbus",	"NL"),	2000,	""),
Model("Airbus A310-200",	"Airbus A300/A310",	240,	6,	1.64,	1.61,	850,	5800,	98698154,	1820,	36,	Manufacturer("Airbus",	"NL"),	2070,	"https://www.norebbo.com/2015/07/airbus-a310-300-blank-illustration-templates/"),
Model("Airbus A310-300",	"Airbus A300/A310",	240,	6,	1.64,	1.54,	850,	8500,	108207069,	1820,	36,	Manufacturer("Airbus",	"NL"),	2380,	"https://www.norebbo.com/2015/07/airbus-a310-300-blank-illustration-templates/"),
Model("Airbus A300-600",	"Airbus A300/A310",	304,	5,	1.61,	1.58,	833,	6125,	102836199,	1820,	36,	Manufacturer("Airbus",	"NL"),	2480,	"https://www.norebbo.com/2018/11/airbus-a300b4-600r-blank-illustration-templates-with-general-electric-engines/"),
Model("Airbus A300B4",	"Airbus A300/A310",	320,	5,	1.47,	1.79,	847,	5375,	99585847,	1820,	32,	Manufacturer("Airbus",	"NL"),	2250,	"https://www.norebbo.com/2018/11/airbus-a300b4-600r-blank-illustration-templates-with-general-electric-engines/"),
Model("Airbus A321neoXLR",	"Airbus A320",	230,	8,	1.7,	1.03,	828,	8400,	223621491,	1820,	36,	Manufacturer("Airbus",	"NL"),	2400,	"https://www.norebbo.com/2018/10/airbus-a321neo-lr-long-range-blank-illustration-templates/"),
Model("Airbus A319neo",	"Airbus A320",	160,	7,	1.12,	1.18,	828,	4900,	128644631,	1820,	20,	Manufacturer("Airbus",	"NL"),	2164,	"https://www.norebbo.com/2017/09/airbus-a319-neo-blank-illustration-templates/"),
Model("Airbus A320neo",	"Airbus A320",	195,	7,	1.03,	1.11,	833,	5646,	167942066,	1820,	24,	Manufacturer("Airbus",	"NL"),	2100,	"https://www.norebbo.com/2017/08/airbus-a320-neo-blank-illustration-templates/"),
Model("Airbus A321neo",	"Airbus A320",	244,	7,	1.06,	1.09,	828,	5450,	201224927,	1820,	36,	Manufacturer("Airbus",	"NL"),	1988,	"https://www.norebbo.com/2017/09/airbus-a321-neo-blank-illustration-templates/"),
Model("Airbus A321neoLR",	"Airbus A320",	230,	7,	1.6,	1.08,	828,	7200,	203081925,	1820,	36,	Manufacturer("Airbus",	"NL"),	2300,	"https://www.norebbo.com/2018/10/airbus-a321neo-lr-long-range-blank-illustration-templates/"),
Model("Airbus A318",	"Airbus A320",	136,	6,	1.35,	1.52,	829,	5569,	61609722,	1820,	8,	Manufacturer("Airbus",	"NL"),	1780,	"https://www.norebbo.com/airbus-a318-blank-illustration-templates-with-pratt-whitney-and-cfm56-engines/"),
Model("Airbus A319",	"Airbus A320",	160,	6,	1.33,	1.44,	830,	4862,	78469289,	1820,	16,	Manufacturer("Airbus",	"NL"),	1850,	"https://www.norebbo.com/2014/05/airbus-a319-blank-illustration-templates/"),
Model("Airbus A320",	"Airbus A320",	195,	6,	1.3,	1.37,	828,	5408,	102525381,	1820,	20,	Manufacturer("Airbus",	"NL"),	2050,	"https://www.norebbo.com/2013/08/airbus-a320-blank-illustration-templates/"),
Model("Airbus A321",	"Airbus A320",	236,	6,	1.29,	1.34,	830,	5184,	128226130,	1820,	24,	Manufacturer("Airbus",	"NL"),	2210,	"https://www.norebbo.com/2014/03/airbus-a321-blank-illustration-templates/"),
Model("Airbus A330-800neo",	"Airbus A330",	406,	7,	1.82,	1.01,	918,	10800,	344795571,	1820,	36,	Manufacturer("Airbus",	"NL"),	2770,	"https://www.norebbo.com/2018/06/airbus-a330-800-neo-blank-illustration-templates/"),
Model("Airbus A330-900neo",	"Airbus A330",	440,	7,	1.85,	0.99,	918,	10939,	377266165,	1820,	36,	Manufacturer("Airbus",	"NL"),	2770,	"https://www.norebbo.com/2018/06/airbus-a330-900-neo-blank-illustration-templates/"),
Model("Airbus A330-200",	"Airbus A330",	406,	6,	1.8,	1.21,	871,	11300,	254694932,	1820,	24,	Manufacturer("Airbus",	"NL"),	2770,	"https://www.norebbo.com/2016/02/airbus-a330-200-blank-illustration-templates-with-pratt-whitney-engines/"),
Model("Airbus A330-300",	"Airbus A330",	440,	6,	1.89,	1.19,	871,	10250,	279561188,	1820,	24,	Manufacturer("Airbus",	"NL"),	2770,	"https://www.norebbo.com/2016/02/airbus-a330-300-blank-illustration-templates-with-all-three-engine-options/"),
Model("Airbus A340-300",	"Airbus A340",	350,	6,	1.99,	1.36,	880,	12824,	176411092,	1820,	36,	Manufacturer("Airbus",	"NL"),	3000,	"https://www.norebbo.com/2016/04/airbus-340-300-and-a340-300x-blank-illustration-templates/"),
Model("Airbus A340-500",	"Airbus A340",	375,	6,	2.24,	1.34,	871,	15345,	185451504,	1820,	36,	Manufacturer("Airbus",	"NL"),	3250,	"https://www.norebbo.com/2016/08/airbus-a340-500-blank-illustration-templates/"),
Model("Airbus A340-600",	"Airbus A340",	440,	6,	2.28,	1.29,	905,	13965,	236716906,	1820,	36,	Manufacturer("Airbus",	"NL"),	3200,	"https://www.norebbo.com/2016/11/airbus-a340-600-blank-illustration-templates/"),
Model("Airbus A350-1000 Sunrise",	"Airbus A350",	420,	9,	2.75,	0.93,	910,	16800,	467089147,	1820,	40,	Manufacturer("Airbus",	"NL"),	2900,	"https://www.norebbo.com/2015/11/airbus-a350-1000-blank-illustration-templates/"),
Model("Airbus A350-1000",	"Airbus A350",	480,	8,	1.96,	0.93,	910,	14535,	459212066,	1820,	40,	Manufacturer("Airbus",	"NL"),	2880,	"https://www.norebbo.com/2015/11/airbus-a350-1000-blank-illustration-templates/"),
Model("Airbus A350-900",	"Airbus A350",	440,	8,	1.84,	0.97,	903,	14147,	417097645,	1820,	36,	Manufacturer("Airbus",	"NL"),	2600,	"https://www.norebbo.com/2013/07/airbus-a350-900-blank-illustration-templates/"),
Model("Airbus A350-900ULR",	"Airbus A350",	374,	8,	2.27,	0.95,	910,	16349,	420141675,	1820,	36,	Manufacturer("Airbus",	"NL"),	2700,	"https://www.norebbo.com/2013/07/airbus-a350-900-blank-illustration-templates/"),
Model("Airbus A380-800",	"Airbus A380",	853,	7,	3.05,	1.04,	925,	14200,	639542517,	1820,	54,	Manufacturer("Airbus",	"NL"),	3200,	"https://www.norebbo.com/2013/06/airbus-a380-800-blank-illustration-templates/"),
Model("Airbus ZeroE Turbofan",	"Airbus ZE",	175,	9,	0.69,	0.59,	795,	1400,	108724412,	1040,	20,	Manufacturer("Airbus",	"NL"),	2100,	""),
Model("Airbus ZeroE Turboprop",	"Airbus ZE",	85,	9,	0.41,	0.56,	674,	1100,	69293135,	1040,	24,	Manufacturer("Airbus",	"NL"),	2000,	""),
Model("Antonov An-148",	"Antonov An",	85,	5,	1.27,	1.83,	835,	3500,	15943380,	1040,	6,	Manufacturer("Antonov",	"UA"),	1100,	""),
Model("Antonov An-72",	"Antonov An",	52,	3,	1.82,	2.74,	700,	2600,	6373702,	1456,	4,	Manufacturer("Antonov",	"UA"),	700,	""),
Model("Antonov An-10A",	"Antonov An",	130,	1,	1.71,	3.14,	680,	1300,	10401454,	1456,	8,	Manufacturer("Antonov",	"UA"),	975,	""),
Model("Antonov An-24",	"Antonov An",	50,	1,	1.51,	3.24,	450,	680,	2794678,	1456,	4,	Manufacturer("Antonov",	"UA"),	970,	""),
Model("ATR 42-600S",	"ATR-Regional",	48,	5,	1.1,	2.79,	535,	1260,	4476814,	1040,	0,	Manufacturer("ATR",	"FR"),	750,	"https://www.norebbo.com/atr-42-blank-illustration-templates/"),
Model("ATR 72-600",	"ATR-Regional",	72,	5,	1.01,	2.38,	510,	1655,	6299282,	1040,	4,	Manufacturer("ATR",	"FR"),	1279,	"https://www.norebbo.com/2017/04/atr-72-blank-illustration-templates/"),
Model("ATR 42-400",	"ATR-Regional",	48,	4,	1.06,	2.86,	484,	1420,	3967151,	1040,	0,	Manufacturer("ATR",	"FR"),	1050,	"https://www.norebbo.com/atr-42-blank-illustration-templates/"),
Model("ATR 72-200",	"ATR-Regional",	68,	4,	1.1,	2.53,	517,	1464,	5655765,	1040,	0,	Manufacturer("ATR",	"FR"),	1211,	"https://www.norebbo.com/2017/04/atr-72-blank-illustration-templates/"),
Model("Aurora D8",	"Aurora D",	188,	9,	1.24,	0.84,	937,	5200,	215561348,	1820,	36,	Manufacturer("Aurora Flight Sciences",	"US"),	2300,	""),
Model("BAe 146-100",	"BAe 146",	82,	4,	1.28,	2.29,	747,	2090,	10826657,	1560,	2,	Manufacturer("BAe",	"GB"),	1195,	"https://www.norebbo.com/2018/11/british-aerospace-bae-146-200-avro-rj85-blank-illustration-templates/"),
Model("BAe 146-200",	"BAe 146",	100,	4,	1.18,	2.13,	747,	1800,	13036486,	1560,	2,	Manufacturer("BAe",	"GB"),	1390,	"https://www.norebbo.com/2018/11/british-aerospace-bae-146-200-avro-rj85-blank-illustration-templates/"),
Model("BAe 146-300",	"BAe 146",	112,	4,	1.17,	2.06,	747,	1650,	15600701,	1560,	2,	Manufacturer("BAe",	"GB"),	1535,	"https://www.norebbo.com/2018/11/british-aerospace-bae-146-200-avro-rj85-blank-illustration-templates/"),
Model("BAe Jetstream 41",	"BAe Jetstream",	29,	7,	1,	3.07,	482,	1210,	3235326,	1820,	0,	Manufacturer("BAe",	"GB"),	1090,	"https://www.norebbo.com/british-aerospace-jetstream-41-blank-illustration-templates/"),
Model("BAe Jetstream 31",	"BAe Jetstream",	19,	6,	1.38,	3.14,	430,	1260,	1772528,	1820,	0,	Manufacturer("BAe",	"GB"),	1100,	"https://www.norebbo.com/british-aerospace-jetstream-41-blank-illustration-templates/"),
Model("Beechcraft 1900D",	"Beechcraft",	17,	6,	1.01,	2.54,	518,	707,	1783451,	1820,	0,	Manufacturer("Beechcraft",	"US"),	1140,	"https://www.norebbo.com/beechcraft-1900d-blank-illustration-templates/"),
Model("Beechcraft B200 Super King Air",	"Beechcraft",	11,	7,	1.06,	3.03,	561,	2800,	1491997,	1820,	0,	Manufacturer("Beechcraft",	"US"),	1020,	"https://www.norebbo.com/beechcraft-b200-king-air-side-view/"),
Model("Boeing 2707",	"Boeing 2707",	247,	8,	4.04,	5.2,	3300,	5600,	364922979,	1664,	42,	Manufacturer("Boeing",	"US"),	3590,	""),
Model("Boeing 707-120",	"Boeing 707",	194,	3,	2.87,	2.61,	952,	5100,	34736218,	2496,	16,	Manufacturer("Boeing",	"US"),	2700,	""),
Model("Boeing 707-320",	"Boeing 707",	189,	3,	3.14,	2.27,	952,	8200,	28030078,	2496,	16,	Manufacturer("Boeing",	"US"),	3150,	"https://www.norebbo.com/boeing-707-320c-blank-illustration-templates/"),
Model("Boeing 720B",	"Boeing 707",	165,	3,	2.07,	2.54,	896,	5700,	27140507,	1976,	12,	Manufacturer("Boeing",	"US"),	2000,	""),
Model("Boeing 727-200",	"Boeing 727",	189,	4,	1.9,	1.97,	811,	3695,	40132565,	2184,	24,	Manufacturer("Boeing",	"US"),	1800,	"https://www.norebbo.com/2018/03/boeing-727-200-blank-illustration-templates/"),
Model("Boeing 727-100",	"Boeing 727",	131,	3,	2.12,	2.2,	960,	3983,	23897395,	2184,	12,	Manufacturer("Boeing",	"US"),	1750,	"https://www.norebbo.com/boeing-727-100-blank-illustration-templates/"),
Model("Boeing 737 MAX 10",	"Boeing 737",	230,	7,	1.14,	1.13,	830,	5400,	177672921,	1820,	36,	Manufacturer("Boeing",	"US"),	2700,	"https://www.norebbo.com/2019/01/737-10-max-side-view/"),
Model("Boeing 737 MAX 7",	"Boeing 737",	172,	7,	1.18,	1.2,	830,	6800,	129243057,	1820,	24,	Manufacturer("Boeing",	"US"),	2100,	"https://www.norebbo.com/2016/07/boeing-737-max-7-blank-illustration-templates/"),
Model("Boeing 737 MAX 8",	"Boeing 737",	189,	7,	1.17,	1.18,	830,	6306,	142866971,	1820,	24,	Manufacturer("Boeing",	"US"),	2035,	"https://www.norebbo.com/2016/07/boeing-737-max-8-blank-illustration-templates/"),
Model("Boeing 737 MAX 9",	"Boeing 737",	220,	7,	1.19,	1.15,	839,	5830,	166662544,	1820,	36,	Manufacturer("Boeing",	"US"),	2600,	"https://www.norebbo.com/2018/05/boeing-737-9-max-blank-illustration-templates/"),
Model("Boeing 737 MAX 8-200",	"Boeing 737",	210,	6,	1.09,	1.16,	839,	4400,	142444617,	1820,	24,	Manufacturer("Boeing",	"US"),	2500,	"https://www.norebbo.com/2016/07/boeing-737-max-8-blank-illustration-templates/"),
Model("Boeing 737-700ER",	"Boeing 737",	140,	6,	1.61,	1.48,	834,	7200,	68823700,	1820,	16,	Manufacturer("Boeing",	"US"),	2060,	"https://www.norebbo.com/2014/04/boeing-737-700-blank-illustration-templates/"),
Model("Boeing 737-800",	"Boeing 737",	188,	6,	1.23,	1.47,	842,	5279,	91988611,	1820,	20,	Manufacturer("Boeing",	"US"),	2230,	"https://www.norebbo.com/2012/11/boeing-737-800-blank-illustration-templates/"),
Model("Boeing 737-900ER",	"Boeing 737",	215,	6,	1.18,	1.42,	844,	5638,	108900360,	1820,	24,	Manufacturer("Boeing",	"US"),	2880,	"https://www.norebbo.com/2016/07/boeing-737-900er-with-split-scimitar-winglets-blank-illustration-templates/"),
Model("Boeing 737-500",	"Boeing 737",	132,	5,	1.66,	1.74,	800,	4249,	45506599,	1976,	12,	Manufacturer("Boeing",	"US"),	2280,	"https://www.norebbo.com/2018/09/boeing-737-500-blank-illustration-templates-with-and-without-blended-winglets/"),
Model("Boeing 737-600",	"Boeing 737",	130,	5,	1.51,	1.62,	834,	4681,	55882545,	1976,	16,	Manufacturer("Boeing",	"US"),	1804,	"https://www.norebbo.com/2018/09/boeing-737-600-blank-illustration-templates/"),
Model("Boeing 737-700",	"Boeing 737",	148,	5,	1.41,	1.53,	834,	5838,	62660519,	1820,	16,	Manufacturer("Boeing",	"US"),	1655,	"https://www.norebbo.com/2014/04/boeing-737-700-blank-illustration-templates/"),
Model("Boeing 737-200",	"Boeing 737",	136,	4,	1.84,	1.94,	780,	3693,	30723112,	1976,	12,	Manufacturer("Boeing",	"US"),	1859,	"https://www.norebbo.com/2018/09/boeing-737-200-blank-illustration-templates/"),
Model("Boeing 737-300",	"Boeing 737",	144,	4,	1.57,	1.8,	800,	4128,	43655712,	1976,	16,	Manufacturer("Boeing",	"US"),	2010,	"https://www.norebbo.com/2018/09/boeing-737-300-blank-illustration-templates/"),
Model("Boeing 737-400",	"Boeing 737",	168,	4,	1.48,	1.58,	800,	4105,	62809800,	1976,	16,	Manufacturer("Boeing",	"US"),	2540,	"https://www.norebbo.com/2018/09/boeing-737-400-blank-illustration-templates/"),
Model("Boeing 737-100",	"Boeing 737",	124,	3,	2.05,	2.05,	780,	2850,	19990120,	1976,	8,	Manufacturer("Boeing",	"US"),	1800,	"https://www.norebbo.com/2018/10/boeing-737-100-blank-illustration-templates/"),
Model("Boeing 747-8i",	"Boeing 747",	645,	7,	2.72,	1.04,	933,	13804,	516855121,	1820,	48,	Manufacturer("Boeing",	"US"),	3180,	"https://www.norebbo.com/2015/12/boeing-747-8i-blank-illustration-templates/"),
Model("Boeing 747-400D",	"Boeing 747",	605,	6,	1.26,	1.37,	933,	2800,	312194056,	1820,	48,	Manufacturer("Boeing",	"US"),	2480,	"https://www.norebbo.com/2013/09/boeing-747-400-blank-illustration-templates/"),
Model("Boeing 747-400ER",	"Boeing 747",	585,	6,	2.85,	1.21,	933,	13804,	389645035,	1820,	48,	Manufacturer("Boeing",	"US"),	3300,	"https://www.norebbo.com/2013/09/boeing-747-400-blank-illustration-templates/"),
Model("Boeing 747SP",	"Boeing 747",	388,	6,	3.32,	1.66,	994,	12051,	164668972,	1820,	36,	Manufacturer("Boeing",	"US"),	2820,	"https://www.norebbo.com/2019/08/boeing-747sp-side-view/"),
Model("Boeing 747-300",	"Boeing 747",	520,	5,	2.87,	1.54,	939,	11127,	218500383,	1820,	48,	Manufacturer("Boeing",	"US"),	2955,	"https://www.norebbo.com/boeing-747-300-side-view/"),
Model("Boeing 747-100",	"Boeing 747",	520,	4,	3.51,	1.89,	907,	8530,	96202471,	1820,	36,	Manufacturer("Boeing",	"US"),	3250,	"https://www.norebbo.com/2019/07/boeing-747-100-side-view/"),
Model("Boeing 747-200",	"Boeing 747",	520,	4,	2.97,	1.64,	907,	10854,	155751225,	1820,	40,	Manufacturer("Boeing",	"US"),	3300,	"https://www.norebbo.com/2019/08/boeing-747-200-side-view/"),
Model("Boeing 757-200",	"Boeing 757",	239,	5,	1.58,	1.42,	854,	6016,	106347537,	1976,	36,	Manufacturer("Boeing",	"US"),	2240,	"https://www.norebbo.com/2015/01/boeing-757-200-blank-illustration-templates/"),
Model("Boeing 757-200ER",	"Boeing 757",	239,	5,	2.21,	1.4,	850,	7378,	106141168,	1976,	36,	Manufacturer("Boeing",	"US"),	2550,	"https://www.norebbo.com/2015/01/boeing-757-200-blank-illustration-templates/"),
Model("Boeing 757-300",	"Boeing 757",	295,	5,	2.01,	1.37,	850,	5742,	137658701,	1976,	36,	Manufacturer("Boeing",	"US"),	2400,	"https://www.norebbo.com/2017/03/boeing-757-300-blank-illustration-templates/"),
Model("Boeing 767-300ER",	"Boeing 767",	290,	6,	2.18,	1.32,	896,	11093,	165430448,	1820,	36,	Manufacturer("Boeing",	"US"),	2650,	"https://www.norebbo.com/2014/07/boeing-767-200-blank-illustration-templates/"),
Model("Boeing 767-200",	"Boeing 767",	245,	5,	1.79,	1.51,	860,	6240,	102287299,	1820,	36,	Manufacturer("Boeing",	"US"),	1900,	"https://www.norebbo.com/2014/07/boeing-767-200-blank-illustration-templates/"),
Model("Boeing 767-200ER",	"Boeing 767",	245,	5,	2.38,	1.41,	896,	11272,	113451499,	1820,	36,	Manufacturer("Boeing",	"US"),	2480,	"https://www.norebbo.com/2014/07/boeing-767-200-blank-illustration-templates/"),
Model("Boeing 767-300",	"Boeing 767",	290,	5,	1.65,	1.44,	860,	6800,	125611439,	1820,	36,	Manufacturer("Boeing",	"US"),	2800,	"https://www.norebbo.com/2014/07/boeing-767-200-blank-illustration-templates/"),
Model("Boeing 767-400ER",	"Boeing 767",	375,	5,	2.08,	1.29,	896,	8600,	165123456,	1820,	36,	Manufacturer("Boeing",	"US"),	3220,	""),
Model("Boeing 777-8",	"Boeing 777",	480,	8,	2.17,	0.9,	896,	15590,	496058566,	1820,	36,	Manufacturer("Boeing",	"US"),	3050,	"https://www.norebbo.com/2019/12/boeing-777-8-side-view/"),
Model("Boeing 777-9",	"Boeing 777",	550,	8,	2.23,	0.88,	896,	13940,	563556309,	1820,	44,	Manufacturer("Boeing",	"US"),	3050,	"https://www.norebbo.com/2019/12/boeing-777-9-side-view/"),
Model("Boeing 777-200ER",	"Boeing 777",	440,	7,	2.45,	1.03,	896,	12680,	334113959,	1820,	36,	Manufacturer("Boeing",	"US"),	3140,	"https://www.norebbo.com/2012/12/boeing-777-200-blank-illustration-templates/"),
Model("Boeing 777-200LR",	"Boeing 777",	440,	7,	2.55,	1.01,	896,	14204,	369824832,	1820,	36,	Manufacturer("Boeing",	"US"),	2940,	"https://www.norebbo.com/2012/12/boeing-777-200-blank-illustration-templates/"),
Model("Boeing 777-300ER",	"Boeing 777",	520,	7,	2.19,	0.99,	945,	13712,	425116644,	1820,	44,	Manufacturer("Boeing",	"US"),	3120,	"https://www.norebbo.com/2014/03/boeing-777-300-blank-illustration-templates/"),
Model("Boeing 777-300",	"Boeing 777",	520,	6,	1.92,	1.07,	945,	6220,	368813919,	1820,	44,	Manufacturer("Boeing",	"US"),	2950,	"https://www.norebbo.com/2014/03/boeing-777-300-blank-illustration-templates/"),
Model("Boeing 787-10 Dreamliner",	"Boeing 787",	440,	7,	1.9,	0.92,	903,	11720,	380449579,	1820,	36,	Manufacturer("Boeing",	"US"),	2800,	"https://www.norebbo.com/2017/06/boeing-787-10-blank-illustration-templates/"),
Model("Boeing 787-8 Dreamliner",	"Boeing 787",	359,	7,	1.93,	0.95,	903,	13593,	301799282,	1820,	36,	Manufacturer("Boeing",	"US"),	2600,	"https://www.norebbo.com/2013/02/boeing-787-8-blank-illustration-templates/"),
Model("Boeing 787-9 Dreamliner",	"Boeing 787",	406,	7,	1.92,	0.93,	903,	13010,	347995745,	1820,	36,	Manufacturer("Boeing",	"US"),	2800,	"https://www.norebbo.com/2014/04/boeing-787-9-blank-illustration-templates/"),
Model("Boeing 787-9 ER",	"Boeing 787",	376,	7,	2.12,	0.91,	903,	15720,	325779277,	1820,	36,	Manufacturer("Boeing",	"US"),	2900,	"https://www.norebbo.com/2014/04/boeing-787-9-blank-illustration-templates/"),
Model("Boeing 797-6",	"Boeing 797",	230,	8,	1.11,	0.93,	890,	7840,	226841100,	1820,	40,	Manufacturer("Boeing",	"US"),	2600,	"https://www.norebbo.com/boeing-797-side-view/"),
Model("Boeing 797-7",	"Boeing 797",	275,	8,	1.32,	0.91,	890,	7200,	271510200,	1820,	40,	Manufacturer("Boeing",	"US"),	2600,	"https://www.norebbo.com/boeing-797-side-view/"),
Model("Boeing Vertol 107-II ",	"Boeing Vertol",	28,	4,	2.01,	3.15,	265,	1020,	2472031,	1300,	4,	Manufacturer("Boeing",	"US"),	1,	""),
Model("Boeing Vertol 234",	"Boeing Vertol",	44,	4,	2.09,	3.11,	269,	1010,	2946121,	1300,	4,	Manufacturer("Boeing",	"US"),	1,	""),
Model("Bombardier CRJ100",	"Bombardier CRJ",	50,	5,	1.3,	2.56,	830,	1820,	9942810,	1820,	0,	Manufacturer("Bombardier",	"CA"),	1920,	"https://www.norebbo.com/2015/04/bombardier-canadair-regional-jet-200-blank-illustration-templates/"),
Model("Bombardier CRJ1000",	"Bombardier CRJ",	104,	5,	1.12,	1.87,	870,	1600,	31009488,	1820,	8,	Manufacturer("Bombardier",	"CA"),	2120,	"https://www.norebbo.com/2019/06/bombardier-crj-1000-side-view/"),
Model("Bombardier CRJ200",	"Bombardier CRJ",	50,	5,	1.26,	2.44,	830,	2813,	10472356,	1820,	0,	Manufacturer("Bombardier",	"CA"),	1920,	"https://www.norebbo.com/2015/04/bombardier-canadair-regional-jet-200-blank-illustration-templates/"),
Model("Bombardier CRJ700",	"Bombardier CRJ",	78,	5,	1.2,	2.2,	828,	2480,	16081201,	1820,	4,	Manufacturer("Bombardier",	"CA"),	1605,	"https://www.norebbo.com/2015/05/bombardier-canadair-regional-jet-700-blank-illustration-templates/"),
Model("Bombardier CRJ900",	"Bombardier CRJ",	90,	5,	1.23,	1.93,	870,	2931,	26313701,	1820,	8,	Manufacturer("Bombardier",	"CA"),	1939,	"https://www.norebbo.com/2016/07/bombardier-canadair-regional-jet-900-blank-illustration-templates/"),
Model("Boom Overture",	"Boom Overture",	128,	10,	2.82,	3.12,	1800,	7870,	133918540,	1664,	100,	Manufacturer("Boom Technology",	"US"),	3048,	""),
Model("CASA CN-235",	"CASA",	40,	4,	1.45,	2.75,	460,	3658,	3367015,	1560,	0,	Manufacturer("CASA",	"ES"),	1204,	""),
Model("CASA C-212 Aviocar",	"CASA",	26,	3,	1.28,	2.87,	354,	2680,	1717990,	1560,	0,	Manufacturer("CASA",	"ES"),	600,	""),
Model("Cessna Caravan",	"Cessna",	14,	5,	0.88,	2.63,	355,	1680,	1077546,	1820,	0,	Manufacturer("Cessna",	"US"),	762,	"https://www.norebbo.com/2017/06/cessna-208-grand-caravan-blank-illustration-templates/"),
Model("Comac C909 ER",	"Comac C909",	90,	5,	1.62,	2.17,	828,	3200,	15738782,	1560,	8,	Manufacturer("COMAC",	"CN"),	1780,	""),
Model("Comac C909 STD",	"Comac C909",	90,	5,	1.29,	2.23,	828,	2040,	14829933,	1560,	8,	Manufacturer("COMAC",	"CN"),	1700,	""),
Model("Comac C919-100 ER",	"Comac C919",	168,	6,	1.36,	1.3,	838,	4300,	89932566,	1560,	16,	Manufacturer("COMAC",	"CN"),	2000,	"https://www.norebbo.com/comac-c919-side-view/"),
Model("Comac C919-100 STD",	"Comac C919",	168,	6,	1.12,	1.38,	838,	2800,	78977610,	1560,	16,	Manufacturer("COMAC",	"CN"),	2000,	"https://www.norebbo.com/comac-c919-side-view/"),
Model("Comac C919-300",	"Comac C919",	198,	6,	1.09,	1.23,	838,	2670,	105751133,	1560,	20,	Manufacturer("COMAC",	"CN"),	2120,	""),
Model("Comac C929-600",	"Comac C929",	405,	7,	1.63,	1.05,	908,	11890,	297990542,	1560,	24,	Manufacturer("COMAC",	"CN"),	2780,	""),
Model("Comac C939",	"Comac C939",	400,	7,	2.03,	1,	908,	10720,	303384315,	1560,	30,	Manufacturer("COMAC",	"CN"),	2780,	""),
Model("Concorde",	"Concorde",	130,	7,	5.07,	4.13,	2158,	6700,	94744439,	1820,	60,	Manufacturer("BAe",	"GB"),	3390,	"https://www.norebbo.com/aerospatiale-bac-concorde-blank-illustration-templates/"),
Model("Dassault Mercure",	"Dassault",	162,	2,	2.08,	2.14,	926,	1684,	19321771,	1820,	12,	Manufacturer("Dassault Aviation",	"FR"),	2100,	""),
Model("McDonnell Douglas MD-11",	"DC-10",	410,	6,	2.63,	1.66,	886,	11963,	132279761,	1820,	24,	Manufacturer("McDonnell Douglas",	"US"),	3050,	"https://www.norebbo.com/mcdonnell-douglas-md-11-blank-illustration-templates-with-ge-engines/"),
Model("McDonnell Douglas DC-10-40",	"DC-10",	390,	5,	2.53,	1.88,	886,	11392,	75429489,	1820,	24,	Manufacturer("McDonnell Douglas",	"US"),	2980,	"https://www.norebbo.com/mcdonnell-douglas-dc-10-30-blank-templates/"),
Model("McDonnell Douglas DC-10-10",	"DC-10",	390,	4,	2.25,	2.11,	876,	6500,	37408695,	1820,	16,	Manufacturer("McDonnell Douglas",	"US"),	2700,	"https://www.norebbo.com/mcdonnell-douglas-dc-10-30-blank-templates/"),
Model("McDonnell Douglas DC-10-30",	"DC-10",	390,	4,	2.44,	1.85,	886,	9400,	74321081,	1820,	20,	Manufacturer("McDonnell Douglas",	"US"),	3220,	"https://www.norebbo.com/mcdonnell-douglas-dc-10-30-blank-templates/"),
Model("McDonnell Douglas DC-8-61",	"DC-8",	259,	3,	2.79,	2.38,	895,	6200,	29034491,	2496,	24,	Manufacturer("McDonnell Douglas",	"US"),	2680,	"https://www.norebbo.com/douglas-dc-8-61-blank-illustration-templates/"),
Model("McDonnell Douglas DC-8-62",	"DC-8",	189,	3,	2.95,	2.22,	895,	9800,	30209283,	2496,	16,	Manufacturer("McDonnell Douglas",	"US"),	2680,	""),
Model("McDonnell Douglas DC-8-63",	"DC-8",	259,	3,	2.85,	2.39,	895,	8100,	37719284,	2496,	24,	Manufacturer("McDonnell Douglas",	"US"),	2680,	"https://www.norebbo.com/douglas-dc-8-73-and-dc-8-73cf-blank-illustration-templates/"),
Model("Douglas DC-8-10",	"DC-8",	177,	2,	2.7,	2.79,	895,	3760,	21477611,	2496,	16,	Manufacturer("Douglas Aircraft Company",	"US"),	2840,	""),
Model("Douglas DC-8-53",	"DC-8",	189,	2,	2.79,	2.63,	895,	8700,	26308744,	2496,	16,	Manufacturer("Douglas Aircraft Company",	"US"),	3200,	"https://www.norebbo.com/douglas-dc-8-53-blank-illustration-templates/"),
Model("Boeing 717-200",	"DC-9",	123,	6,	1.26,	1.74,	822,	2397,	35531944,	1820,	12,	Manufacturer("Boeing",	"US"),	1675,	"https://www.norebbo.com/2017/06/boeing-717-200-blank-illustration-templates/"),
Model("McDonnell Douglas MD-90",	"DC-9",	171,	5,	1.28,	1.79,	830,	1400,	39808382,	1560,	20,	Manufacturer("McDonnell Douglas",	"US"),	2134,	"https://www.norebbo.com/2018/02/mcdonnell-douglas-md-90-blank-illustration-templates/"),
Model("McDonnell Douglas MD-90ER",	"DC-9",	163,	5,	1.32,	1.75,	830,	3300,	43890350,	1560,	20,	Manufacturer("McDonnell Douglas",	"US"),	2134,	"https://www.norebbo.com/2018/02/mcdonnell-douglas-md-90-blank-illustration-templates/"),
Model("McDonnell Douglas MD-81",	"DC-9",	161,	4,	1.48,	1.95,	830,	2600,	30221345,	1560,	12,	Manufacturer("McDonnell Douglas",	"US"),	2070,	"https://www.norebbo.com/2015/02/mcdonnell-douglas-md-80-blank-illustration-templates/"),
Model("McDonnell Douglas MD-85",	"DC-9",	135,	4,	1.51,	1.98,	830,	2500,	23978782,	1560,	12,	Manufacturer("McDonnell Douglas",	"US"),	1840,	"https://www.norebbo.com/2015/02/mcdonnell-douglas-md-80-blank-illustration-templates/"),
Model("McDonnell Douglas DC-9-50",	"DC-9",	139,	3,	1.63,	2.07,	804,	3030,	13416958,	1040,	4,	Manufacturer("McDonnell Douglas",	"US"),	2100,	"https://www.norebbo.com/dc-9-50-side-view/"),
Model("McDonnell Douglas DC-9-10",	"DC-9",	92,	2,	1.81,	2.21,	965,	2367,	8807513,	1040,	4,	Manufacturer("McDonnell Douglas",	"US"),	1816,	""),
Model("McDonnell Douglas DC-9-30",	"DC-9",	115,	2,	1.76,	2.14,	804,	2778,	9657714,	1040,	4,	Manufacturer("McDonnell Douglas",	"US"),	1900,	"https://www.norebbo.com/mcdonnell-douglas-dc-9-30-templates/"),
Model("De Havilland Q400",	"DHC Dash",	86,	5,	1.02,	2,	562,	2040,	12636468,	1560,	8,	Manufacturer("De Havilland Canada",	"CA"),	1885,	"https://www.norebbo.com/bombardier-dhc-8-402-q400-blank-illustration-templates/"),
Model("De Havilland DHC-8-100",	"DHC Dash",	39,	4,	1.31,	2.59,	448,	1889,	4850242,	1820,	2,	Manufacturer("De Havilland Canada",	"CA"),	950,	"https://www.norebbo.com/2018/01/de-havilland-dhc-8-200-dash-8-blank-illustration-templates/"),
Model("De Havilland DHC-8-200",	"DHC Dash",	39,	4,	1.31,	2.49,	448,	2084,	4899658,	1820,	2,	Manufacturer("De Havilland Canada",	"CA"),	1000,	"https://www.norebbo.com/2018/01/de-havilland-dhc-8-200-dash-8-blank-illustration-templates/"),
Model("De Havilland DHC-8-300",	"DHC Dash",	50,	4,	1.35,	2.29,	450,	1711,	6191489,	1820,	4,	Manufacturer("De Havilland Canada",	"CA"),	1085,	"https://www.norebbo.com/2018/05/de-havilland-dhc-8-300-blank-illustration-templates/"),
Model("De Havilland DHC-8-400",	"DHC Dash",	68,	4,	1.31,	2.08,	667,	1980,	10343292,	1560,	4,	Manufacturer("De Havilland Canada",	"CA"),	1085,	"https://www.norebbo.com/bombardier-dhc-8-402-q400-blank-illustration-templates/"),
Model("De Havilland DHC-7-100",	"DHC Dash",	50,	3,	1.35,	2.78,	428,	1300,	5541337,	1820,	4,	Manufacturer("De Havilland Canada",	"CA"),	620,	""),
Model("Dornier 328eco",	"Dornier",	44,	9,	0,	0.43,	600,	590,	38563457,	1820,	8,	Manufacturer("Dornier",	"DE"),	1367,	""),
Model("Dornier 1128",	"Dornier",	128,	7,	1.4,	1.79,	923,	2640,	58849332,	1976,	12,	Manufacturer("Dornier",	"DE"),	1550,	""),
Model("Dornier 328-110",	"Dornier",	33,	7,	1.27,	2.38,	620,	820,	5672440,	1820,	6,	Manufacturer("Dornier",	"DE"),	1088,	"https://www.norebbo.com/2019/01/dornier-328-110-blank-illustration-templates/"),
Model("Dornier 328JET",	"Dornier",	44,	7,	1.7,	2.12,	740,	1150,	8446775,	1820,	6,	Manufacturer("Dornier",	"DE"),	1367,	"https://www.norebbo.com/2019/01/fairchild-dornier-328jet-illustrations/"),
Model("Dornier 728",	"Dornier",	80,	7,	1.64,	1.95,	1000,	2340,	28380982,	1820,	6,	Manufacturer("Dornier",	"DE"),	1463,	""),
Model("Dornier 928",	"Dornier",	110,	7,	1.55,	1.82,	951,	2180,	48588109,	1976,	8,	Manufacturer("Dornier",	"DE"),	1513,	""),
Model("Heart ES-30",	"Heart",	30,	9,	0,	0.232,	370,	400,	26633977,	1560,	4,	Manufacturer("Heart Aerospace",	"SE"),	920,	""),
Model("Embraer E175-E2",	"Embraer E-Jet",	88,	7,	1.14,	1.46,	833,	3650,	49597651,	1560,	6,	Manufacturer("Embraer",	"BR"),	1800,	"https://www.norebbo.com/2019/03/e175-e2-side-view/"),
Model("Embraer E190-E2",	"Embraer E-Jet",	114,	7,	1.18,	1.34,	833,	4620,	76645137,	1560,	8,	Manufacturer("Embraer",	"BR"),	1465,	"https://www.norebbo.com/2019/03/e190-e2-blank-side-view/"),
Model("Embraer E195-E2",	"Embraer E-Jet",	146,	7,	1.16,	1.32,	833,	4511,	100167164,	1560,	12,	Manufacturer("Embraer",	"BR"),	1605,	"https://www.norebbo.com/2019/03/embraer-e195-e2-side-view/"),
Model("Embraer E170",	"Embraer E-Jet",	72,	6,	1.45,	1.61,	797,	2400,	21177237,	1560,	2,	Manufacturer("Embraer",	"BR"),	1438,	"https://www.norebbo.com/embraer-erj-175-templates-with-the-new-style-winglets/"),
Model("Embraer E175 LR",	"Embraer E-Jet",	78,	6,	1.57,	1.58,	797,	2900,	25619119,	1560,	4,	Manufacturer("Embraer",	"BR"),	1420,	"https://www.norebbo.com/2015/10/embraer-erj-175-templates-with-the-new-style-winglets/"),
Model("Embraer E190",	"Embraer E-Jet",	100,	6,	1.41,	1.69,	829,	3685,	29627454,	1560,	8,	Manufacturer("Embraer",	"BR"),	1650,	"https://www.norebbo.com/2015/06/embraer-190-blank-illustration-templates/"),
Model("Embraer ERJ 145XR",	"Embraer E-Jet",	50,	6,	1.54,	2.17,	854,	3450,	9075857,	1560,	4,	Manufacturer("Embraer",	"BR"),	1720,	"https://www.norebbo.com/2018/04/embraer-erj-145xr-blank-illustration-templates/"),
Model("Embraer ERJ 135LR",	"Embraer E-Jet",	37,	5,	1.48,	2.22,	833,	2854,	5966174,	1560,	4,	Manufacturer("Embraer",	"BR"),	1580,	"https://www.norebbo.com/2018/05/embraer-erj-135-blank-illustration-templates/"),
Model("Airbus H225 Eurocopter",	"Eurocopter",	24,	8,	1.46,	3.27,	262,	857,	3070732,	1560,	8,	Manufacturer("Airbus",	"NL"),	1,	""),
Model("Fokker 100",	"Fokker",	109,	4,	1.29,	1.68,	845,	2220,	27837299,	1300,	4,	Manufacturer("Fokker",	"NL"),	1550,	"https://www.norebbo.com/2018/07/fokker-100-f-28-0100-blank-illustration-templates/"),
Model("Fokker 50",	"Fokker",	56,	3,	1.6,	2.56,	500,	2310,	4003187,	1300,	2,	Manufacturer("Fokker",	"NL"),	1550,	"https://www.norebbo.com/fokker-70-blank-illustration-templates/"),
Model("Fokker 60",	"Fokker",	64,	3,	1.64,	2.51,	515,	2950,	4843026,	1300,	2,	Manufacturer("Fokker",	"NL"),	1550,	"https://www.norebbo.com/fokker-70-blank-illustration-templates/"),
Model("Fokker 70",	"Fokker",	85,	3,	1.2,	1.72,	845,	2840,	16833842,	1300,	6,	Manufacturer("Fokker",	"NL"),	1480,	"https://www.norebbo.com/fokker-70-blank-illustration-templates/"),
Model("Fokker F27 Friendship",	"Fokker",	44,	2,	1.31,	2.97,	460,	1380,	2750045,	1300,	4,	Manufacturer("Fokker",	"NL"),	1550,	""),
Model("Harbin Y-12",	"Harbin Y",	12,	5,	0.84,	2.73,	250,	1140,	1763519,	1664,	4,	Manufacturer("Harbin",	"CN"),	720,	""),
Model("Airlander 10",	"HAV Airlander",	130,	10,	0,	0.13,	178,	3400,	27518057,	780,	20,	Manufacturer("HAV Airlander",	"GB"),	100,	""),
Model("Ilyushin Il-62M",	"Ilyushin Il-62",	186,	4,	2.93,	2.35,	900,	7000,	28919039,	1820,	12,	Manufacturer("Ilyushin",	"RU"),	2300,	""),
Model("Ilyushin Il-18",	"Ilyushin Il-18",	120,	0,	2.09,	2.48,	625,	6500,	5635963,	1300,	4,	Manufacturer("Ilyushin",	"RU"),	1350,	""),
Model("Ilyushin Il-96-400M",	"Ilyushin Il-96",	436,	6,	2.78,	1.72,	870,	9600,	106674818,	1456,	24,	Manufacturer("UAC",	"RU"),	2600,	""),
Model("Ilyushin Il-96-300",	"Ilyushin Il-96",	300,	5,	2.98,	2.02,	870,	10100,	45235237,	1456,	18,	Manufacturer("Ilyushin",	"RU"),	3200,	""),
Model("Ilyushin Il-86",	"Ilyushin Il-96",	350,	4,	2.46,	2.19,	870,	4400,	42492976,	1456,	16,	Manufacturer("Ilyushin",	"RU"),	2800,	""),
Model("Lockheed L-1011-200",	"Lockheed TriStar",	380,	5,	2.34,	1.79,	954,	6100,	98203759,	1820,	16,	Manufacturer("Lockheed",	"US"),	2560,	"https://www.norebbo.com/lockheed-l-1011-1-blank-illustration-templates/"),
Model("Lockheed L-1011-500",	"Lockheed TriStar",	330,	5,	2.61,	1.69,	972,	9344,	113571710,	1820,	24,	Manufacturer("Lockheed",	"US"),	2865,	"https://www.norebbo.com/2015/03/lockheed-l-1011-500-blank-illustration-templates/"),
Model("Lockheed L-1011-100",	"Lockheed TriStar",	380,	4,	2.39,	1.97,	963,	4200,	84565535,	2080,	16,	Manufacturer("Lockheed",	"US"),	2560,	"https://www.norebbo.com/lockheed-l-1011-1-blank-illustration-templates/"),
Model("Mil Mi-26",	"Mil",	78,	0,	2.03,	2.89,	255,	870,	2712323,	780,	0,	Manufacturer("Mil",	"RU"),	1,	""),
Model("Bombardier Global 5000",	"Modern Business Jet",	30,	10,	1.87,	2.21,	934,	9630,	10433102,	1820,	8,	Manufacturer("Bombardier",	"CA"),	1689,	"https://www.norebbo.com/bombardier-global-5000-blank-illustration-templates/"),
Model("Bombardier Global 7500",	"Modern Business Jet",	35,	10,	1.92,	1.92,	1080,	14260,	18978828,	1820,	8,	Manufacturer("Bombardier",	"CA"),	1768,	"https://www.norebbo.com/bombardier-global-7500-side-view/"),
Model("Cessna Citation X",	"Modern Business Jet",	20,	10,	1.85,	2.54,	900,	6050,	6168066,	1820,	8,	Manufacturer("Cessna",	"US"),	1600,	"https://www.norebbo.com/cessna-citation-x-template/"),
Model("Gulfstream G650ER",	"Modern Business Jet",	40,	10,	1.72,	1.91,	966,	13890,	20939809,	2080,	8,	Manufacturer("Gulfstream",	"US"),	1920,	"https://www.norebbo.com/gulfstream-g650er-template/"),
Model("Boeing 307 Stratoliner",	"Post-War Props",	60,	1,	2.08,	2.97,	357,	3850,	3411900,	1508,	8,	Manufacturer("Boeing",	"US"),	805,	""),
Model("Boeing 377 Stratocruiser",	"Post-War Props",	117,	1,	2.9,	2.71,	480,	6760,	7001425,	1956,	12,	Manufacturer("Boeing",	"US"),	1920,	""),
Model("Bristol Britannia",	"Post-War Props",	139,	1,	2.59,	2.42,	575,	6400,	12926252,	1820,	2,	Manufacturer("BAe",	"GB"),	2225,	""),
Model("Lockheed Constellation L-749",	"Post-War Props",	81,	1,	3.13,	2.34,	555,	8039,	4004609,	1820,	12,	Manufacturer("Lockheed",	"US"),	1524,	""),
Model("Douglas DC-3",	"Post-War Props",	32,	0,	1.78,	3.12,	333,	1400,	1172592,	3224,	2,	Manufacturer("Douglas Aircraft Company",	"US"),	701,	"https://www.norebbo.com/douglas-dc-3-blank-illustration-templates/"),
Model("Saab 2000",	"Saab Regional",	58,	5,	1.42,	2.23,	608,	2868,	7197498,	1820,	2,	Manufacturer("Saab",	"SE"),	1252,	"https://www.norebbo.com/saab-340b-blank-illustration-templates/"),
Model("Saab 90 Scandia",	"Saab Regional",	32,	2,	1.5,	2.65,	340,	2650,	2130593,	1820,	0,	Manufacturer("Saab",	"SE"),	850,	""),
Model("Sikorsky S-92",	"Sikorsky",	25,	10,	1.34,	3.26,	290,	998,	3835902,	1560,	2,	Manufacturer("Sikorsky",	"US"),	1,	""),
Model("Sikorsky S-76",	"Sikorsky",	12,	9,	1.38,	3.22,	287,	761,	1325133,	1560,	2,	Manufacturer("Sikorsky",	"US"),	1,	""),
Model("Sud Aviation Caravelle 12",	"Sud Aviation Caravelle",	140,	2,	2.42,	2.3,	810,	3200,	12254570,	1300,	4,	Manufacturer("Sud Aviation",	"FR"),	1310,	""),
Model("Sud Aviation Caravelle III",	"Sud Aviation Caravelle",	80,	2,	2.64,	2.52,	845,	2100,	7165628,	1300,	4,	Manufacturer("Sud Aviation",	"FR"),	1270,	""),
Model("Sukhoi Superjet 100",	"Sukhoi",	108,	6,	1.26,	1.76,	844,	4578,	33030100,	1300,	16,	Manufacturer("UAC",	"RU"),	1400,	"https://www.norebbo.com/2016/02/sukhoi-ssj-100-blank-illustration-templates/"),
Model("Sukhoi Superjet 130NG",	"Sukhoi",	130,	6,	1.16,	1.91,	844,	4008,	32159567,	1300,	16,	Manufacturer("UAC",	"RU"),	1400,	"https://www.norebbo.com/2016/02/sukhoi-ssj-100-blank-illustration-templates/"),
Model("Sukhoi KR-860",	"Sukhoi",	920,	5,	3.64,	1.65,	980,	8800,	285063051,	1300,	58,	Manufacturer("Sukhoi",	"RU"),	3500,	""),
Model("Tupolev Tu-134",	"Tupolev 124",	72,	2,	2.91,	2.98,	850,	2200,	5518445,	1456,	12,	Manufacturer("Tupolev",	"RU"),	1450,	""),
Model("Tupolev Tu-154M",	"Tupolev 154",	180,	3,	1.79,	2.34,	850,	5280,	20973407,	1664,	12,	Manufacturer("Tupolev",	"RU"),	1250,	"https://www.norebbo.com/tupolev-tu-154-side-view/"),
Model("Tupolev Tu-154",	"Tupolev 154",	180,	2,	1.82,	2.78,	850,	2500,	16089865,	1664,	12,	Manufacturer("Tupolev",	"RU"),	1140,	"https://www.norebbo.com/tupolev-tu-154-side-view/"),
Model("Tupolev Tu-204–300",	"Tupolev 204",	156,	6,	1.67,	1.76,	820,	6452,	34784701,	1300,	18,	Manufacturer("Tupolev",	"RU"),	1650,	"https://www.norebbo.com/tupolev-tu-204-100-blank-illustration-templates/"),
Model("Tupolev Tu-204-120",	"Tupolev 204",	210,	4,	1.5,	1.91,	820,	3800,	31820723,	1300,	18,	Manufacturer("Tupolev",	"RU"),	1870,	"https://www.norebbo.com/tupolev-tu-204-100-blank-illustration-templates/"),
Model("Vickers VC10",	"Vickers",	150,	3,	2.96,	2.38,	930,	9410,	17073503,	1976,	12,	Manufacturer("Vickers-Armstrongs",	"GB"),	2520,	""),
Model("Xi'an MA700",	"Xi'an Turboprop",	86,	6,	1.42,	2.28,	637,	1620,	12648350,	1300,	8,	Manufacturer("AVIC",	"CN"),	840,	""),
Model("Xi'an MA60",	"Xi'an Turboprop",	60,	4,	1.48,	2.53,	519,	1180,	6283317,	1300,	4,	Manufacturer("AVIC",	"CN"),	730,	""),
Model("Xi'an MA600",	"Xi'an Turboprop",	62,	3,	1.42,	2.41,	514,	1430,	6177077,	1300,	4,	Manufacturer("AVIC",	"CN"),	750,	""),
Model("Yakovlev MC-21-210",	"Yakovlev MC-21",	165,	5,	1.11,	1.39,	870,	2700,	71269001,	1300,	24,	Manufacturer("UAC",	"RU"),	1250,	""),
Model("Yakovlev MC-21-310",	"Yakovlev MC-21",	211,	5,	1.16,	1.44,	870,	2400,	84822948,	1300,	30,	Manufacturer("UAC",	"RU"),	1644,	"https://www.norebbo.com/irkut-mc-21-300/"),
Model("Yakovlev MC-21-310 LR",	"Yakovlev MC-21",	190,	5,	1.25,	1.41,	880,	4200,	85489178,	1300,	30,	Manufacturer("UAC",	"RU"),	1880,	"https://www.norebbo.com/irkut-mc-21-300/"),
Model("Yakovlev MC-21-410",	"Yakovlev MC-21",	230,	5,	1.3,	1.4,	870,	2200,	95228384,	1300,	36,	Manufacturer("UAC",	"RU"),	1700,	""),
  )
  val modelByName = models.map { model => (model.name, model) }.toMap
}