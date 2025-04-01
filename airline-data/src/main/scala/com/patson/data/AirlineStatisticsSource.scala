package com.patson.data

import java.sql.Connection
import com.patson.data.Constants._
import com.patson.model._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object AirlineStatisticsSource {

  def saveAirlineStats(stats: List[AirlineStat]) = {
    val queryString = s"REPLACE INTO $AIRLINE_STATISTICS_TABLE (airline, cycle, period, tourists, elites, business, total, codeshares, rask, cask, satisfaction, load_factor, on_time, hub_dominance) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)";
    val connection = Meta.getConnection()
    try {
      connection.setAutoCommit(false)
      val preparedStatement = connection.prepareStatement(queryString)
      stats.foreach { entry =>
        preparedStatement.setInt(1, entry.airlineId)
        preparedStatement.setInt(2, entry.cycle)
        preparedStatement.setInt(3, entry.period.id)
        preparedStatement.setInt(4, entry.tourists)
        preparedStatement.setInt(5, entry.elites)
        preparedStatement.setInt(6, entry.business)
        preparedStatement.setInt(7, entry.total)
        preparedStatement.setInt(8, entry.codeshares)
        preparedStatement.setDouble(9, entry.RASK)
        preparedStatement.setDouble(10, entry.CASK)
        preparedStatement.setDouble(11, entry.satisfaction)
        preparedStatement.setDouble(12, entry.loadFactor)
        preparedStatement.setDouble(13, entry.onTime)
        preparedStatement.setDouble(14, entry.hubDominance)
        preparedStatement.executeUpdate()
      }
      preparedStatement.close()
      connection.commit()
    } finally {
      connection.close()
    }
  }

  def saveAirlineStat(stat: AirlineStat) = {
    val queryString = s"REPLACE INTO $AIRLINE_STATISTICS_TABLE (airline, cycle, period, tourists, elites, business, total, codeshares, rask, cask, satisfaction, load_factor, on_time, hub_dominance) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)";
    val connection = Meta.getConnection()
    try {
      connection.setAutoCommit(false)
      val preparedStatement = connection.prepareStatement(queryString)
      preparedStatement.setInt(1, stat.airlineId)
      preparedStatement.setInt(2, stat.cycle)
      preparedStatement.setInt(3, stat.period.id)
      preparedStatement.setInt(4, stat.tourists)
      preparedStatement.setInt(5, stat.elites)
      preparedStatement.setInt(6, stat.business)
      preparedStatement.setInt(7, stat.total)
      preparedStatement.setInt(8, stat.codeshares)
      preparedStatement.setDouble(9, stat.RASK)
      preparedStatement.setDouble(10, stat.CASK)
      preparedStatement.setDouble(12, stat.satisfaction)
      preparedStatement.setDouble(13, stat.loadFactor)
      preparedStatement.setDouble(14, stat.onTime)
      preparedStatement.setDouble(15, stat.hubDominance)
      preparedStatement.executeUpdate()
      preparedStatement.close()
      connection.commit()
    } finally {
      connection.close()
    }
  }

  def deleteIncomesBefore(cycleAndBefore : Int, period : Period.Value) = {
    val connection = Meta.getConnection()
    try {
      connection.setAutoCommit(false)
      var deleteStatement = connection.prepareStatement("DELETE FROM " + AIRLINE_STATISTICS_TABLE + " WHERE cycle <= ? AND period = ?")
      deleteStatement.setInt(1, cycleAndBefore)
      deleteStatement.setInt(2, period.id)
      deleteStatement.executeUpdate()

      deleteStatement.close()
      connection.commit
    } finally {
      connection.close()
    }
  }

  def loadAirlineStatsForAirlines(airlines: List[Airline]): List[AirlineStat] = {
    if (airlines.isEmpty) {
      List.empty
    } else {
      val connection = Meta.getConnection()
      val airlineIds = airlines.map(_.id)
      val lastCycle = CycleSource.loadCycle() - 1
      val queryString = new StringBuilder(s"SELECT * FROM $AIRLINE_STATISTICS_TABLE WHERE cycle = ? AND airline IN (");
      for (i <- 0 until airlineIds.size - 1) {
        queryString.append("?,")
      }
      queryString.append("?)") //last item has no comma

      try {
        val preparedStatement = connection.prepareStatement(queryString.toString())
        preparedStatement.setInt(1, lastCycle)
        for (i <- 0 until airlineIds.size) {
          preparedStatement.setObject(i + 2, airlineIds(i))
        }

        val resultSet = preparedStatement.executeQuery()
        val airlineStats = ListBuffer[AirlineStat]()

        while (resultSet.next()) {
          val airlineId = resultSet.getInt("airline")
          val cycle = resultSet.getInt("cycle")
          val period = Period(resultSet.getInt("period"))
          val tourists = resultSet.getInt("tourists")
          val elites = resultSet.getInt("elites")
          val business = resultSet.getInt("business")
          val total = resultSet.getInt("total")
          val codeshares = resultSet.getInt("codeshares")
          val rask = resultSet.getDouble("rask")
          val cask = resultSet.getDouble("cask")
          val satisfaction = resultSet.getDouble("satisfaction")
          val loadFactor = resultSet.getDouble("load_factor")
          val onTime = resultSet.getDouble("on_time")
          val hubDominance = resultSet.getDouble("hub_dominance")
          airlineStats += AirlineStat(airlineId, cycle, period, tourists, elites, business, total, codeshares, rask, cask, satisfaction, loadFactor, onTime, hubDominance)
        }

        airlineStats.toList
      } finally {
        connection.close()
      }
    }
  }

  def loadAirlineStat(airlineId: Int, cycle: Int): Option[AirlineStat] = {
    val airlineStats = loadAirlineStatsByCriteria(List(("airline", airlineId), ("cycle", cycle)))
    if (airlineStats.length > 0) {
      Some(airlineStats(0))
    } else {
      None
    }
  }

  def loadAirlineStats(airlineId: Int): List[AirlineStat] = {
    loadAirlineStatsByCriteria(List(("airline", airlineId)))
  }

  def loadAirlineStatsByCycle(cycle: Int): List[AirlineStat] = {
    loadAirlineStatsByCriteria(List(("cycle", cycle)))
  }

  def loadAirlineStatsByCriteria(criteria: List[(String, Any)]) = {
    val connection = Meta.getConnection()
    val airlineStats = ListBuffer[AirlineStat]()
    try {
      val airlineStatQuery = getAirlineStatistics(connection, criteria)
      val resultSet = airlineStatQuery.executeQuery()

      while (resultSet.next()) {
        val airlineId = resultSet.getInt("airline")
        val cycle = resultSet.getInt("cycle")
        val period = Period(resultSet.getInt("period"))
        val tourists = resultSet.getInt("tourists")
        val elites = resultSet.getInt("elites")
        val business = resultSet.getInt("business")
        val total = resultSet.getInt("total")
        val codeshares = resultSet.getInt("codeshares")
        val rask = resultSet.getDouble("rask")
        val cask = resultSet.getDouble("cask")
        val satisfaction = resultSet.getDouble("satisfaction")
        val loadFactor = resultSet.getDouble("load_factor")
        val onTime = resultSet.getDouble("on_time")
        val hubDominance = resultSet.getDouble("hub_dominance")
        airlineStats += AirlineStat(airlineId, cycle, period, tourists, elites, business, total, codeshares, rask, cask, satisfaction, loadFactor, onTime, hubDominance)
      }

      airlineStats.toList
    } finally {
      connection.close()
    }
  }

  def getAirlineStatistics(connection: Connection, criteria: List[(String, Any)]) = {
    val queryString = new StringBuilder(s"SELECT * FROM $AIRLINE_STATISTICS_TABLE")

    if (!criteria.isEmpty) {
      queryString.append(" WHERE ")
      for (i <- 0 until criteria.size - 1) {
        queryString.append(criteria(i)._1 + " = ? AND ")
      }
      queryString.append(criteria.last._1 + " = ?")
    }

    val preparedStatement = connection.prepareStatement(queryString.toString())

    for (i <- 0 until criteria.size) {
      preparedStatement.setObject(i + 1, criteria(i)._2)
    }
    preparedStatement
  }

}
