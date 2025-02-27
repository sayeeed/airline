package models

import com.patson.model.{AirlineType, Airport, Loan}
import com.patson.model.airplane.Airplane

case class Profile(name : String, airlineType : AirlineType.Value, description : String, rule : List[String] = List(""), cash : Int, airport : Airport, reputation : Int = 0, quality : Int = 5, airplanes : List[Airplane] = List.empty, loan : Option[Loan] = None)
