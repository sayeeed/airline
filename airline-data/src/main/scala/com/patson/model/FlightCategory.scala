package com.patson.model

object FlightCategory extends Enumeration {
    type FlightCategory = Value
    val DOMESTIC, INTERNATIONAL = Value


    val label = (category : FlightCategory.Value) => category match {
        case DOMESTIC => "Domestic"
        case INTERNATIONAL => "International"
    }
}
