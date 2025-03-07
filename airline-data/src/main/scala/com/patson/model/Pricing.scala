package com.patson.model

import com.patson.model.FlightCategory.FlightCategory

/**
 * Cost base model
 */
object Pricing {
  val modifierBrackets: Map[LinkClass, List[(Int, Double)]] = Map(
    DISCOUNT_ECONOMY  -> List((400, 0.12), (1000, 0.071), (6000, 0.06), (Int.MaxValue, 0.1)),
    ECONOMY           -> List((400, 0.16), (1000, 0.075), (6000, 0.079), (9000, 0.08), (Int.MaxValue, 0.13)),
    BUSINESS          -> List((400, 0.29), (1000, 0.196), (6000, 0.15), (Int.MaxValue, 0.19)),
    FIRST             -> List((400, 1.11), (1000, 0.248), (6000, 0.315), (Int.MaxValue, 0.42))
  )
  val INTERNATIONAL_PRICE_MULTIPLIER = 1.05

  def computeStandardPrice(link : Transport, linkClass : LinkClass) : Int = {
    val flightCategory = Computation.getFlightCategory(link.from, link.to)
    computeStandardPrice(link.distance, flightCategory, linkClass)
  }
  def computeStandardPrice(distance: Int, flightCategory: FlightCategory.Value, linkClass: LinkClass) : Int = {
    var remainDistance = distance
    var price = 15.0
    for (priceBracket <- modifierBrackets(linkClass) if(remainDistance > 0)) {
      if (priceBracket._1 >= remainDistance) {
        price += remainDistance * priceBracket._2
      } else {
        price += priceBracket._1.toDouble * priceBracket._2
      }
      remainDistance -= priceBracket._1
    }
    price = if (flightCategory == FlightCategory.INTERNATIONAL) {
      price * INTERNATIONAL_PRICE_MULTIPLIER
    } else {
      price
    }
    
    (price * 1.1).toInt //increase the standard price by 10%
  }
  
  def computeStandardPriceForAllClass(distance: Int, flightCategory: FlightCategory.Value) : LinkClassValues = {
    val priceByLinkClass : List[(LinkClass, Int)] = LinkClass.values.map { linkClass =>
      (linkClass, computeStandardPrice(distance, flightCategory, linkClass))
    }
    LinkClassValues.getInstanceByMap(priceByLinkClass.toMap)
  }

}