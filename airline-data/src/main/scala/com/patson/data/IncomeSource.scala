package com.patson.data
import com.patson.data.Constants._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import java.sql.DriverManager
import com.patson.model.airplane.Airplane
import java.sql.PreparedStatement
import com.patson.model._
import java.sql.Statement
import scala.collection.mutable.HashSet
import java.sql.Connection
import scala.collection.mutable.HashMap
import com.patson.MainSimulation
 


object IncomeSource {
  val FULL_LOAD = Map(DetailType.AIRLINE -> true, DetailType.AIRPORT -> true, DetailType.AIRPLANE -> true)
  val SIMPLE_LOAD = Map(DetailType.AIRLINE -> false, DetailType.AIRPORT -> false, DetailType.AIRPLANE -> false)
  val ID_LOAD : Map[DetailType.Type, Boolean] = Map.empty

  def saveIncomes(incomes: List[AirlineIncome]) = {
     //open the hsqldb
    val connection = Meta.getConnection()
    val incomePreparedStatement = connection.prepareStatement("REPLACE INTO " + INCOME_TABLE + "(airline, profit, revenue, expense, stock_price, total_value, period, cycle) VALUES(?,?,?,?,?,?,?,?)")
    val linksPreparedStatement = connection.prepareStatement("REPLACE INTO " + LINKS_INCOME_TABLE + "(airline, profit, revenue, expense, ticket_revenue, airport_fee, fuel_cost, fuel_tax, crew_cost, inflight_cost, delay_compensation, maintenance_cost, lounge_cost, depreciation, period, cycle) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)")
    val transactionsPreparedStatement = connection.prepareStatement("REPLACE INTO " + TRANSACTIONS_INCOME_TABLE + "(airline, profit, revenue, expense, capital_gain, create_link, buy_back, prize, period, cycle) VALUES(?,?,?,?,?,?,?,?,?,?)")
    val othersPreparedStatement = connection.prepareStatement("REPLACE INTO " + OTHERS_INCOME_TABLE + "(airline, profit, revenue, expense, loan_interest, base_upkeep, advertisement, lounge_upkeep, lounge_cost, lounge_income, asset_expense, asset_revenue, fuel_profit, depreciation, overtime_compensation, period, cycle) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)")
    
    try {
      connection.setAutoCommit(false)
      incomes.foreach { income =>
          val period = income.period
          incomePreparedStatement.setInt(1, income.airlineId)
          incomePreparedStatement.setLong(2, income.profit)
          incomePreparedStatement.setLong(3, income.revenue)
          incomePreparedStatement.setLong(4, income.expense)
          incomePreparedStatement.setDouble(5, income.stockPrice)
          incomePreparedStatement.setLong(6, income.totalValue)
          incomePreparedStatement.setInt(7, period.id)
          incomePreparedStatement.setInt(8, income.cycle)
          incomePreparedStatement.addBatch()
          
          linksPreparedStatement.setInt(1, income.airlineId)
          linksPreparedStatement.setLong(2, income.links.profit)
          linksPreparedStatement.setLong(3, income.links.revenue)
          linksPreparedStatement.setLong(4, income.links.expense)
          linksPreparedStatement.setLong(5, income.links.ticketRevenue)
          linksPreparedStatement.setLong(6, income.links.airportFee)
          linksPreparedStatement.setLong(7, income.links.fuelCost)
          linksPreparedStatement.setLong(8, income.links.fuelTax)
          linksPreparedStatement.setLong(9, income.links.crewCost)
          linksPreparedStatement.setLong(10, income.links.inflightCost)
          linksPreparedStatement.setLong(11, income.links.delayCompensation)
          linksPreparedStatement.setLong(12, income.links.maintenanceCost)
          linksPreparedStatement.setLong(13, income.links.loungeCost)
          linksPreparedStatement.setLong(14, income.links.depreciation)
          linksPreparedStatement.setInt(15, period.id)
          linksPreparedStatement.setInt(16, income.cycle)
          linksPreparedStatement.addBatch()
          
          
          transactionsPreparedStatement.setInt(1, income.airlineId)
          transactionsPreparedStatement.setLong(2, income.transactions.profit)
          transactionsPreparedStatement.setLong(3, income.transactions.revenue)
          transactionsPreparedStatement.setLong(4, income.transactions.expense)
          transactionsPreparedStatement.setLong(5, income.transactions.capitalGain)
          transactionsPreparedStatement.setLong(6, income.transactions.createLink)
          transactionsPreparedStatement.setLong(7, income.transactions.buyBack)
          transactionsPreparedStatement.setLong(8, income.transactions.prize)
          transactionsPreparedStatement.setInt(9, period.id)
          transactionsPreparedStatement.setInt(10, income.cycle)
          transactionsPreparedStatement.addBatch()
          
          
          
          othersPreparedStatement.setInt(1, income.airlineId)
          othersPreparedStatement.setLong(2, income.others.profit)
          othersPreparedStatement.setLong(3, income.others.revenue)
          othersPreparedStatement.setLong(4, income.others.expense)
          othersPreparedStatement.setLong(5, income.others.loanInterest)
          othersPreparedStatement.setLong(6, income.others.baseUpkeep)
          othersPreparedStatement.setLong(7, income.others.advertisement)
          othersPreparedStatement.setLong(8, income.others.loungeUpkeep)
          othersPreparedStatement.setLong(9, income.others.loungeCost)
          othersPreparedStatement.setLong(10, income.others.loungeIncome)
          othersPreparedStatement.setLong(11, income.others.assetExpense)
          othersPreparedStatement.setLong(12, income.others.assetRevenue)
          othersPreparedStatement.setLong(13, income.others.fuelProfit)
          othersPreparedStatement.setLong(14, income.others.depreciation)
          othersPreparedStatement.setLong(15, income.others.overtimeCompensation)
          othersPreparedStatement.setInt(16, period.id)
          othersPreparedStatement.setInt(17, income.cycle)
          othersPreparedStatement.addBatch()
          
          
      }
      
      incomePreparedStatement.executeBatch()
      incomePreparedStatement.close()
      linksPreparedStatement.executeBatch()
      linksPreparedStatement.close()
      transactionsPreparedStatement.executeBatch()
      transactionsPreparedStatement.close()
      othersPreparedStatement.executeBatch()
      othersPreparedStatement.close()
      connection.commit
    } finally {
      connection.close()
    }
  }
  
  def deleteIncomes(cycle : Int, period : Period.Value) = {
    val connection = Meta.getConnection()
    try {
      connection.setAutoCommit(false)
      var deleteStatement = connection.prepareStatement("DELETE FROM " + INCOME_TABLE + " WHERE cycle = ? AND period = ?")
      deleteStatement.setInt(1, cycle)
      deleteStatement.setInt(2, period.id)
      deleteStatement.executeUpdate()
      
      deleteStatement = connection.prepareStatement("DELETE FROM " + LINKS_INCOME_TABLE + " WHERE cycle = ? AND period = ?")
      deleteStatement.setInt(1, cycle)
      deleteStatement.setInt(2, period.id)
      deleteStatement.executeUpdate()
      
      deleteStatement = connection.prepareStatement("DELETE FROM " + TRANSACTIONS_INCOME_TABLE + " WHERE cycle = ? AND period = ?")
      deleteStatement.setInt(1, cycle)
      deleteStatement.setInt(2, period.id)
      deleteStatement.executeUpdate()
      
      deleteStatement = connection.prepareStatement("DELETE FROM " + OTHERS_INCOME_TABLE + " WHERE cycle = ? AND period = ?")
      deleteStatement.setInt(1, cycle)
      deleteStatement.setInt(2, period.id)
      deleteStatement.executeUpdate()

      deleteStatement.close()
      connection.commit
    } finally {
      connection.close()
    }
  }
  
  
  def deleteIncomesBefore(cycleAndBefore : Int, period : Period.Value) = {
    val connection = Meta.getConnection()
    try {
      connection.setAutoCommit(false)
      var deleteStatement = connection.prepareStatement("DELETE FROM " + INCOME_TABLE + " WHERE cycle <= ? AND period = ?")
      deleteStatement.setInt(1, cycleAndBefore)
      deleteStatement.setInt(2, period.id)
      deleteStatement.executeUpdate()
      
      deleteStatement = connection.prepareStatement("DELETE FROM " + LINKS_INCOME_TABLE + " WHERE cycle <= ? AND period = ?")
      deleteStatement.setInt(1, cycleAndBefore)
      deleteStatement.setInt(2, period.id)
      deleteStatement.executeUpdate()
      
      deleteStatement = connection.prepareStatement("DELETE FROM " + TRANSACTIONS_INCOME_TABLE + " WHERE cycle <= ? AND period = ?")
      deleteStatement.setInt(1, cycleAndBefore)
      deleteStatement.setInt(2, period.id)
      deleteStatement.executeUpdate()
      
      deleteStatement = connection.prepareStatement("DELETE FROM " + OTHERS_INCOME_TABLE + " WHERE cycle <= ? AND period = ?")
      deleteStatement.setInt(1, cycleAndBefore)
      deleteStatement.setInt(2, period.id)
      deleteStatement.executeUpdate()

      deleteStatement.close()
      connection.commit
    } finally {
      connection.close()
    }
  }
  
  def loadIncomeByAirline(airlineId : Int, cycle: Int, period : Period.Value) : Option[AirlineIncome] = {
    val incomes = loadIncomeByCriteria(List(("airline", airlineId), ("cycle", cycle), ("period", period.id)))
    if (incomes.length > 0) {
      Some(incomes(0)) 
    } else {
      None
    }
  }
  
  def loadIncomesByAirline(airlineId : Int) : List[AirlineIncome] = {
    loadIncomeByCriteria(List(("airline", airlineId)))
  }

  def loadAllByCycle(cycle : Int) : List[AirlineIncome] = {
    loadIncomeByCriteria(List(("cycle", cycle), ("period", 0)))
  }
  
  
  def loadIncomeByCriteria(criteria : List[(String, Any)]) = {
    val connection = Meta.getConnection()
    val incomes = ListBuffer[AirlineIncome]()  
    try {
      val incomeStatement = getIncomeStatement(connection, criteria)
      val resultSet = incomeStatement.executeQuery()
      
      while (resultSet.next()) {
          val airlineId = resultSet.getInt("i.airline")
          val totalProfit = resultSet.getLong("i.profit")
          val totalRevenue = resultSet.getLong("i.revenue") 
          val totalExpense = resultSet.getLong("i.expense")
          val stockPrice = resultSet.getDouble("i.stock_price")
          val totalValue = resultSet.getLong("i.total_value")
          val period = Period(resultSet.getInt("i.period"))
          val cycle = resultSet.getInt("i.cycle")
           
          //should need that many queries...
          val linksBalance = LinksIncome(airlineId = resultSet.getInt("l.airline"),
                          profit = resultSet.getLong("l.profit"),
                          revenue = resultSet.getLong("l.revenue"),
                          expense = resultSet.getLong("l.expense"),
                          ticketRevenue = resultSet.getLong("l.ticket_revenue"),
                          airportFee = resultSet.getLong("l.airport_fee"),
                          fuelCost = resultSet.getLong("l.fuel_cost"),
                          fuelTax = resultSet.getLong("l.fuel_tax"),
                          crewCost = resultSet.getLong("l.crew_cost"),
                          inflightCost = resultSet.getLong("l.inflight_cost"),
                          delayCompensation = resultSet.getLong("l.delay_compensation"),
                          maintenanceCost= resultSet.getLong("l.maintenance_cost"),
                          loungeCost= resultSet.getLong("l.lounge_cost"),
                          depreciation = resultSet.getLong("l.depreciation"),
                          period = Period(resultSet.getInt("l.period")),
                          cycle = resultSet.getInt("l.cycle"))
                         
            
          val transactionsBalance = TransactionsIncome(airlineId,
                          profit = resultSet.getLong("t.profit"),
                          revenue = resultSet.getLong("t.revenue"),
                          expense = resultSet.getLong("t.expense"),
                          capitalGain = resultSet.getLong("t.capital_gain"),
                          createLink = resultSet.getLong("t.create_link"),
                          buyBack = resultSet.getLong("t.buy_back"),
                          prize = resultSet.getLong("t.prize"),
                          period = Period(resultSet.getInt("t.period")),
                          cycle = resultSet.getInt("t.cycle"))
              
          
          val othersBalance = OthersIncome(airlineId,
                         profit = resultSet.getLong("o.profit"),
                         revenue = resultSet.getLong("o.revenue"), 
                         expense = resultSet.getLong("o.expense"), 
                         loanInterest = resultSet.getLong("o.loan_interest"), 
                         baseUpkeep = resultSet.getLong("o.base_upkeep"),
                         overtimeCompensation = resultSet.getLong("o.overtime_compensation"),
                         advertisement = resultSet.getLong("o.advertisement"),
                         loungeUpkeep = resultSet.getLong("o.lounge_upkeep"),
                         loungeCost = resultSet.getLong("o.lounge_cost"),
                         loungeIncome = resultSet.getLong("o.lounge_income"),
                         assetExpense = resultSet.getLong("o.asset_expense"),
                         assetRevenue = resultSet.getLong("o.asset_revenue"),
                         fuelProfit = resultSet.getLong("o.fuel_profit"),
                         depreciation = resultSet.getLong("o.depreciation"),
                         period = Period(resultSet.getInt("o.period")),
                         cycle = resultSet.getInt("o.cycle"))
          
          incomes += AirlineIncome(airlineId, totalProfit, totalRevenue, totalExpense, stockPrice, totalValue, linksBalance, transactionsBalance, othersBalance, period, cycle)
      }
       
       incomes.toList
    } finally {
      connection.close()
    }
  }
  
  def getIncomeStatement(connection: Connection, criteria : List[(String, Any)]) = {
    val queryString = new StringBuilder("SELECT i.*, l.*, t.*, o.* FROM " + INCOME_TABLE + " i")
      
    val onClause = new StringBuilder()
    if (!criteria.isEmpty) {
      for (i <- 0 until criteria.size) {
        onClause.append("i." + criteria(i)._1 + " = ? AND ")
      }
    }
    
    queryString.append(" JOIN " + LINKS_INCOME_TABLE + " l ON " + onClause + " i.airline = l.airline AND i.period = l.period AND i.cycle = l.cycle" +
                       " JOIN " + TRANSACTIONS_INCOME_TABLE + " t ON i.airline = t.airline AND i.period = t.period AND i.cycle = t.cycle" +
                       " JOIN " + OTHERS_INCOME_TABLE + " o ON i.airline = o.airline AND i.period = o.period AND i.cycle = o.cycle")
                       
    val preparedStatement = connection.prepareStatement(queryString.toString())
    
    for (i <- 0 until criteria.size) {
        preparedStatement.setObject(i + 1, criteria(i)._2)
    }
    preparedStatement
  }
  
  object DetailType extends Enumeration {
    type Type = Value
    val AIRPORT, AIRLINE, AIRPLANE = Value
  }
}
