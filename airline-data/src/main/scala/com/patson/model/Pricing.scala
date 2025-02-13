package com.patson.model

import com.patson.model.FlightCategory.FlightCategory

/**
 * Cost base model
 */
object Pricing {
  //base 10
  //200 km = 10 + 40
  //1000 km = 50 + 100 = 150  (800 * 0.125) // 250
  //2000 km = 150 + 100 = 250  (1000 * 0.1) // 350
  //10000 km = 150 + 900 = 1050  (9000 * 0.1) // 750
  val modifierBrackets: Map[LinkClass, List[(Int, Double)]] = Map(
    DISCOUNT_ECONOMY -> List((200, 0.2),(2000, 0.16),(Int.MaxValue, 0.1)),
    ECONOMY -> List((200, 0.2),(2000, 0.15),(Int.MaxValue, 0.1)),
    BUSINESS -> List((200, 0.2),(800, 0.125),(5000, 0.12),(Int.MaxValue, 0.11)),
    FIRST -> List((200, 0.2),(800, 0.125),(5000, 0.1),(Int.MaxValue, 0.12))
  )
  val INTERNATIONAL_PRICE_MULTIPLIER = 1.05

  def computeStandardPrice(link : Transport, linkClass : LinkClass) : Int = {
    val flightCategory = Computation.getFlightCategory(link.from, link.to)
    computeStandardPrice(link.distance, flightCategory, linkClass)
  }
  def computeStandardPrice(distance: Int, flightCategory: FlightCategory.Value, linkClass: LinkClass) : Int = {
    var remainDistance = distance
    var price = linkClass.basePrice.toDouble
    for (priceBracket <- modifierBrackets(linkClass) if(remainDistance > 0)) {
      if (priceBracket._1 >= remainDistance) {
        price += remainDistance * priceBracket._2
      } else {
        price += priceBracket._1 * priceBracket._2
      }
      remainDistance -= priceBracket._1
    }
    price = (if (flightCategory == FlightCategory.INTERNATIONAL) {
      price * INTERNATIONAL_PRICE_MULTIPLIER
    } else {
     price
    } * linkClass.priceMultiplier).toInt
    
    (price * 1.20).toInt //increase the standard price by 20%
  }
  
  def computeStandardPriceForAllClass(distance: Int, flightCategory: FlightCategory.Value) : LinkClassValues = {
    val priceByLinkClass : List[(LinkClass, Int)] = LinkClass.values.map { linkClass =>
      (linkClass, computeStandardPrice(distance, flightCategory, linkClass))
    }
    LinkClassValues.getInstanceByMap(priceByLinkClass.toMap)
  }

}