package com.patson.model

object PassengerType extends Enumeration {
    val BUSINESS, TOURIST, OLYMPICS, ELITE, TRAVELER = Value

    val label = (paxType : PassengerType.Value) => paxType match {
        case BUSINESS => "Business"
        case TOURIST => "Tourist"
        case OLYMPICS => "Olympic"
        case ELITE => "Elite"
        case TRAVELER => "Traveler"
    }
    val priceAdjust = (paxType: PassengerType.Value) => paxType match {
        case BUSINESS => 1.15
        case TOURIST => 1.0
        case _ => 1.05
    }
    val routeCostTolerance = (paxType: PassengerType.Value) => paxType match {
        case TRAVELER => 1.4
        case BUSINESS => 1.2
        case _ => 1.15
    }
}

case class PassengerGroup(fromAirport : Airport, preference : FlightPreference, passengerType : PassengerType.Value)

