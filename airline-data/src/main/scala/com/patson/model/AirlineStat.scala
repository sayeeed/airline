package com.patson.model

import com.patson.model.Period.Period

case class AirlineStat(
                        var airlineId : Int,
                        var cycle : Int,
                        var period: Period,
                        var tourists : Int,
                        var elites : Int,
                        var business : Int,
                        var total : Int,
                        var codeshares : Int,
                        var RASK: Double,
                        var CASK: Double,
                        var satisfaction: Double,
                        var loadFactor: Double,
                        var onTime: Double,
                        var hubDominance: Double
                      )

//intermediate data object
case class AirlinePaxStat(
                        var tourists : Int,
                        var elites : Int,
                        var business : Int,
                        var total : Int,
                        var codeshares : Int,
                      )