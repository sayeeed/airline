package com.patson.init

import com.patson.model.airplane.Model
import com.patson.data.airplane.ModelSource

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object AirplaneModelInitializer {
  def populateAirplaneModels() = {
    ModelSource.deleteAllModels()
  
    ModelSource.saveModels(Model.models)
  }
}