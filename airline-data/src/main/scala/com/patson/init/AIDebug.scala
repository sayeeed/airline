package com.patson.init

import com.patson.AISimulation

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object AIDebug extends App {
  mainFlow
  
  def mainFlow() = {
    AISimulation.simulateAIAirlines(3)

    Await.result(actorSystem.terminate(), Duration.Inf)
  }
}
