package com.patson.model

case class LinkConsumptionDetails(link : Transport, fuelCost : Int, fuelTax : Int,crewCost : Int, airportFees: Int, inflightCost : Int, delayCompensation: Int, maintenanceCost : Int, depreciation : Int, loungeCost : Int, revenue : Int, profit : Int, satisfaction : Double, cycle : Int, var id : Int = 0) extends IdObject
