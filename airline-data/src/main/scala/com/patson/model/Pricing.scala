package com.patson.model

import com.patson.model.FlightCategory.FlightCategory

/**
 * Cost base model
 */
object Pricing {
  val modifierBrackets: Map[LinkClass, List[(Int, Double)]] = Map(
    DISCOUNT_ECONOMY  -> List((400, 0.06), (1000, 0.067), (6000, 0.065), (Int.MaxValue, 0.07)),
    ECONOMY           -> List((400, 0.11), (1000, 0.070), (6000, 0.076), (Int.MaxValue, 0.09)),
    BUSINESS          -> List((400, 0.28), (1000, 0.217), (6000, 0.17), (Int.MaxValue, 0.225)),
    FIRST             -> List((400, 1.24), (1000, 0.305), (6000, 0.42), (Int.MaxValue, 0.55))
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