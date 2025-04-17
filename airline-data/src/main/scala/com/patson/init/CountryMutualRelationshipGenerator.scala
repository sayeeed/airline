package com.patson.init

import com.patson.data.CountrySource

import scala.collection.mutable
import scala.collection.mutable.Map

object CountryMutualRelationshipGenerator extends App {
  /**
   * - Affliation is mutal between all members. Affliations only "upgrade" relations, never decrease
   * - A "5" relation creates a "home market"
   *
   * Some relations set in the computation function!
   */
  lazy val OECDish = List("CA","US","FR","DE","AT","CH","IT","GB","ES","NL","BE","PL","DK","SE","IE","JP","KR","AU","SG")

  lazy val AFFILIATIONS = List(

    Affiliation("OECD Allies", 3, OECDish),

    Affiliation("EU", 4, List(
      "BE", "GR", "LT", "PT", "BG", "ES", "LU", "RO", "CZ", "FR", "HU", "SI", "DK", "HR", "MT", "SK", "DE", "IT", "NL", "FI", "EE", "CY", "AT", "SE", "IE", "LV", "PL"
    )),
    Affiliation("Scandinavia / NATO", 4, List("DK","NO","SE","FI","IS") ++ List(
      "IE", "GB", "NL", "BE", "DE", "PL", "ES", "PT", "GR", "IT", "US", "CA"
    )),
//    Affiliation("France", 5, List(
//      "FR", "GF", "GP", "MF", "MQ", "PM", "BL"
//    )),
//    Affiliation("France Pacific", 5, List(
//      "FR", "RE", "NC", "PF"
//    )),
    //other UK territories are one-one relationships
    Affiliation("UK Caribbean", 5, List(
      "GB", "TC", "KY", "VG", "BM", "BA"
//      "GB", "TC", "KY", "BM", "VG", "MS", "AI", "BM"
    )),
//    Affiliation("Denmark", 5, List(
//      "DK", "GL", "FO"
//    )),
    Affiliation("Netherlands", 5, List(
      "NL", "AW", "CW", "SX"
    )),
    Affiliation("US Anglo Caribbean", 3, List(
      "US", "CA", "PR", "GB", "FR", "BM", "AW", "AG", "BB", "BS", "GY", "JM", "KY", "TC", "TT", "VG", "VI"
//      "US", "CA", "PR", "GB", "AW", "AG", "AI", "BB", "BS", "GY", "JM", "KY", "MS", "TC", "TT", "VG", "VI"
    )),
    Affiliation("US", 5, List(
      "US", "VI", "PR"
    )),
    Affiliation("US Pacific", 5, List(
      "US", "MH", "FM", "AS", "GU", "MP", "PW"
    )),
    Affiliation("NAFTA", 4, List(
      "US", "CA", "MX", "PR"
    )),
    Affiliation("COFTA", 3, List(
      "US", "GT", "HN", "SV", "NI", "PR", "DO"
    )),
    Affiliation("NZ", 5, List(
      "NZ", "CK", "NU"
    )),
    Affiliation("ANZAC common market", 4, List(
      "AU", "NZ", "CK", "NU"
    )),
    Affiliation("Arab Free Trade Area", 3, List(
      "SA", "EG", "BH", "QA", "AE", "KW", "JO", "LB", "OM", "SD", "IQ", "LY", "MA", "TN", "SY"
    )),
    Affiliation("GCC+", 4, List(
      "SA", "EG", "BH", "QA", "AE", "KW", "JO", "OM"
    )),
    Affiliation("EAC", 4, List(
      "KE", "UG", "SS", "RW", "BI", "TZ"
    )),
    Affiliation("Comunidad Andina", 4, List(
      "BO", "EC", "PE", "CO"
    )),
    Affiliation("ALBA", 3, List(
      "VE", "CU", "BO", "NI"
    )),
//    Affiliation("ECOWAS", 2, List(
//      "BJ", "BF", "CV", "CI", "GM", "GH", "GN", "GW", "LR", "NE", "NG", "SN", "TG"
//    )),
//    Affiliation("ECCAS", 2, List(
//      "AO", "BI", "CM", "TD", "CD", "GQ", "GA", "CG", "RW", "ST"
//    )),
    Affiliation("SADC+", 3, List(
      "ZA", "BW", "SZ", "LS", "NA", "ZM", "ZW", "AO"
    )),
    Affiliation("SADC", 4, List(
      "ZA", "BW", "SZ", "LS", "NA"
    )),
    Affiliation("ASEAN", 3, List(
      "BN", "KH", "ID", "LA", "MY", "PH", "SG", "TH", "VN"
    )),
//    Affiliation("CPTPP", 3, List(
//      "AU", "BN", "CA", "CL", "JP", "MY", "MX", "NZ", "PE", "SG", "VN", "GB"
//    )),
    Affiliation("China", 5, List(
      "CN", "HK", "MO"
    )),
    Affiliation("CIS", 3, List(
      "RU", "BY", "KZ", "KG", "TJ", "UZ", "AZ", "AM"
    ))
  )
  lazy val FRIENDSHIPS = List(
    //pacific
    Relation("AU", Direction.BI, 3, List(
      "SG","TH","ID","PH"
    )),
    Relation("NZ", Direction.BI, 3, List(
      "SG","ID","JP","HK"
    )),
    Relation("NZ", Direction.BI, 4, List(
      "AU","GB","DE","US","CA","JP","KR","MY","TH"
    )),
    Relation("FJ", Direction.BI, 2, List(
      "AU", "NZ", "US", "FR", "WS", "TV", "TO", "KI", "MH"
    )),
    Relation("PG", Direction.BI, 2, List(
      "AU", "GU", "PH", "JP", "CN"
    )),
    Relation("WS", Direction.BI, 4, List(
      "AU", "NZ", "AS", "CK"
    )),
//    Relation("PF", Direction.BI, 4, List(
//      "AU", "NZ", "CA", "US"
//    )),
    Relation("FM", Direction.BI, 4, List(
      "JP"
    )),
    Relation("GU", Direction.BI, 4, List(
      "KR", "JP", "PH"
    )),
    Relation("FM", Direction.BI, 2, List(
      "CN", "AU"
    )),
    Relation("AS", Direction.BI, 4, List(
      "AU", "NZ", "US", "FR"
    )),
    Relation("KI", Direction.BI, 3, List(
      "MH", "AU"
    )),
//    Relation("NC", Direction.BI, 4, List(
//      "AU", "NZ", "AS", "US"
//    )),
//    Relation("NC", Direction.BI, 2, List(
//      "FJ", "VU"
//    )),
    Relation("VU", Direction.BI, 4, List(
      "FR", "AS"
    )),
    //e-asia
    Relation("CN", Direction.BI, 4, List(
      "KH", "PK", "RU"
    )),
    Relation("CN", Direction.BI, 2, List(
      "KP", "ET", "DJ", "HU", "VN", "IR", "SG", "MY", "MM", "NP"
    )),
    Relation("HK", Direction.BI, 2, List(
      "KR", "JP", "GB"
    )),
    Relation("JP", Direction.BI, 2, List(
      "PE", "BR", "IN", "VN", "TH", "FJ", "PG", "SB"
    )),
    Relation("KR", Direction.BI, 4, List(
      "SG", "TW", "US"
    )),
    Relation("KR", Direction.BI, 3, List(
      "JP"
    )),
    Relation("KR", Direction.BI, 2, List(
      "MY", "TH", "VN"
    )),
    Relation("TW", Direction.BI, 4, List(
      "NL","CA","US","JP","HK"
    )),
    Relation("TW", Direction.BI, 3, List(
      "DE","GB","JP","KR","SG","AU"
    )),
    //se-asia
    Relation("ID", Direction.BI, 3, List(
      "AU", "NZ", "JP"
    )),
    Relation("ID", Direction.BI, 2, List(
      "IN", "CN", "KR", "AE", "SA", "NL"
    )),
    Relation("PH", Direction.BI, 3, List(
      "US", "JP"
    )),
    Relation("PH", Direction.BI, 2, List(
      "KR", "AU", "AE", "SA"
    )),
    Relation("VN", Direction.BI, 2, List(
      "TW", "JP", "DE", "SA", "GB"
    )),
    Relation("TH", Direction.BI, 3, List(
      "CN", "JP", "KR", "US"
    )),
    Relation("TH", Direction.TO, 2, List(
      "RU","IL","FR","DE","GB","NL","BE","DK","SE","NO","FI","AU","NZ","US"
    )),
    Relation("MY", Direction.TO, 2, List(
      "RU", "IL", "FR", "DE", "GB", "NL", "BE", "DK", "SE", "NO", "FI", "AU", "NZ", "US", "JP", "TW", "KR"
    )),
    //south-asia
    Relation("IN", Direction.BI, 4, List(
      "BT"
    )),
    Relation("IN", Direction.BI, 3, List(
      "NP", "LK", "BD", "AE"
    )),
    Relation("IN", Direction.BI, 2, List(
      "GB", "FR", "MY", "MM", "US", "CA", "SG", "SA", "KW", "OM", "QA", "PH", "ID", "JP", "ZA", "KR"
    )),
    Relation("IN", Direction.BI, 1, List(
      "KE", "IL", "AF", "RU"
    )),
    Relation("BD", Direction.BI, 2, List(
      "SG", "MY", "AE", "SA", "GB", "US", "CA", "CN", "IT"
    )),
    Relation("PK", Direction.BI, 2, List(
      "AE", "SA", "GB", "US"
    )),
    //w-asia
    Relation("GE", Direction.BI, 3, List(
      "IL", "TR", "UA", "AE", "AZ", "DE"
    )),
    //europe
    Relation("RU", Direction.BI, 3, List(
      "EG", "KP", "TM", "IR", "IQ"
    )),
    Relation("RU", Direction.BI, 2, List(
      "SA", "ZA"
    )),
    Relation("TR", Direction.BI, 4, List(
      "AZ"
    )),
    Relation("TR", Direction.BI, 3, OECDish ++ List(
      "QA", "KZ", "UZ"
    )),
    Relation("TR", Direction.BI, 2, List(
      "RU", "UA", "GE", "IQ", "IR", "PK"
    )),
    Relation("GR", Direction.TO, 3, List(
      "DE", "BG", "GB", "IT", "FR"
    )),
    Relation("RS", Direction.BI, 2, List(
      "DE", "FR", "RU", "TR", "RO", "CY"
    )),
    Relation("BA", Direction.BI, 2, List(
      "DE", "FR", "IT", "HU"
    )),
    Relation("FR", Direction.BI, 4, List(
      "GB", "US", "CA"
    )),
    Relation("FR", Direction.BI, 2, List(
      "TN", "DZ", "DJ", "MA", "SN", "CI"
    )),
    Relation("IT", Direction.BI, 2, List(
      "TN", "MA", "DZ", "IL"
    )),
    Relation("CH", Direction.BI, 4, List(
      "FR","DE","AT","IT","ES","NL","BE","DK","SE"
    )),
//    Relation("SH", Direction.BI, 3, List(
//      "FK", "NA"
//    )),
    Relation("GB", Direction.BI, 4, List(
      "AU", "NZ"
    )),
    Relation("PT", Direction.BI, 3, List(
      "CV"
    )),
    //mena
    Relation("IL", Direction.BI, 1, List(
      "IN", "RO", "PL", "GB"
    )),
    Relation("IL", Direction.BI, 3, List(
      "CA", "US", "BG", "SG"
    )),
    Relation("SA", Direction.BI, 1, List(
      "SG", "CN", "IN", "BD", "NG"
    )),
    Relation("SA", Direction.BI, 2, List(
      "BN", "DZ", "LY", "TR", "TN"
    )),
    Relation("SA", Direction.BI, 3, List(
      "MY", "PK"
    )),
    Relation("AE", Direction.BI, 3, List(
      "MY", "SG", "IN", "PK", "LY"
    )),
    Relation("QA", Direction.BI, 3, List(
      "MY", "SG", "IN", "PK", "LY"
    )),
    Relation("EG", Direction.BI, 2, List(
      "DE", "FR", "UA", "IT", "GB", "LY"
    )),
    Relation("TN", Direction.BI, 3, List(
      "FR", "LY", "AE", "SA"
    )),
    Relation("MA", Direction.BI, 5, List(
      "EH"
    )),
    Relation("MA", Direction.BI, 3, List(
      "FR", "ES", "GB", "DE", "US"
    )),
    //africa, sub
    Relation("ZA", Direction.BI, 2, List(
      "ZW", "MZ", "GB", "DE", "US", "AU", "MW", "IN", "TZ", "KE", "CN", "TZ", "ET"
    )),
    Relation("AO", Direction.BI, 2, List(
      "PT", "MZ", "CV", "KE", "ET", "NG"
    )),
    Relation("TZ", Direction.BI, 2, List(
      "MW", "MZ", "ZA", "ZW", "KM"
    )),
    Relation("ET", Direction.BI, 4, List(
      "DJ"
    )),
    Relation("ET", Direction.BI, 2, List(
      "EG", "CN", "UG", "RW", "AE"
    )),
    Relation("ET", Direction.BI, 1, List(
      "IT", "SD", "US", "KE"
    )),
    //americas
    Relation("US", Direction.BI, 4, List(
      "JP", "KR", "TW", "AU", "NZ", "BM", "BS"
    )),
    Relation("US", Direction.BI, 3, List(
      "KW", "QA"
    )),
    Relation("US", Direction.BI, 2, List(
      "AE", "AR"
    )),
    Relation("US", Direction.BI, 1, List(
      "SA", "EG"
    )),
//    Relation("GP", Direction.BI, 4, List(
//      "CA", "US", "PR"
//    )),
//    Relation("MQ", Direction.BI, 4, List(
//      "CA", "US", "PR"
//    )),
    Relation("CO", Direction.BI, 3, List(
      "US", "PE", "EC", "PA", "CL"
    )),
    Relation("CO", Direction.BI, 2, List(
      "MX", "BR", "BO", "ES"
    )),
    Relation("VE", Direction.BI, 2, List(
      "IR", "RU", "BR", "CN"
    )),
    Relation("PE", Direction.BI, 2, List(
      "CL", "BO", "EC", "CO", "MX", "US", "CN", "JP", "ES"
    )),
    Relation("BR", Direction.BI, 3, List(
      "PT", "AR", "BO", "PY", "UY", "PE", "CL"
    )),
    Relation("BR", Direction.BI, 2, List(
      "BO", "CO", "MX", "US", "CN", "ZA", "AO", "JP", "DE", "CV"
    )),
    Relation("BR", Direction.BI, 1, List(
      "IN", "FR"
    )),
    Relation("AR", Direction.BI, 2, List(
      "UY", "PY", "ES"
    )),
    Relation("CL", Direction.BI, 3, List(
      "PE","AR","US","CA","PA" //chile has FTAs with everyone
    )),
    Relation("CL", Direction.BI, 2, List(
      "MX", "JP", "KR", "CN", "HK", "AU", "NZ", "ES"
    )),
  )
  lazy val ENMITIES = List(
    Relation("KP", Direction.BI, -3, OECDish),
    Relation("KP", Direction.BI, -2, List(
      "BN", "KH", "ID", "LA", "MY", "PH", "SG", "TH", "VN", "BR", "IN", "ZA", "TR"
    )),
    Relation("RU", Direction.BI, -1, List(
      "HU"
    )),
    Relation("RU", Direction.BI, -3, List(
      "UA", "US", "CA", "JP", "KR", "TW", "AU", "NZ", "GB", "ME", "IE", "RO", "BG", "CY", "AT", "BE", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HR", "IS", "IT", "LT", "LU", "LV", "MT", "NL", "NO", "PL", "PT", "SI", "SK", "MD", "ES", "SE", "CH"
    )),
    Relation("BY", Direction.BI, -2, OECDish ++ List(
      "UA", "MD", "EE", "LT", "LV", "BG"
    )),
    Relation("IR", Direction.BI, -1, OECDish ++ List(
      "EG", "PK"
    )),
    Relation("IR", Direction.BI, -3, List(
      "SA"
    )),
    Relation("IL", Direction.BI, -2, List(
      "IQ", "YE", "LY", "SD", "KW", "SA", "EG"
    )),
    Relation("IL", Direction.BI, -4, List(
      "IR", "SY", "LB"
    )),
    Relation("PK", Direction.BI, -2, List(
      "IN", "IR"
    )),
    Relation("TR", Direction.BI, -1, List(
      "GR", "CY", "AM"
    )),
    Relation("US", Direction.BI, -3, List(
      "IR", "CU"
    )),
    Relation("CN", Direction.BI, 0, List(
      "KR"
    )),
    Relation("CN", Direction.BI, -1, List(
      "US", "IN", "AU", "JP", "TW"
    )),
    Relation("AM", Direction.BI, -3, List(
      "AZ"
    )),
    Relation("VE", Direction.TO, -1, List(
      "US", "CO", "GY", "SR", "AR"
    ))
  )


  mainFlow()



  def mainFlow() = {
    var mutualRelationshipMap = getCountryMutualRelationship()
//    val mutualRelationshipPatchMap = getCountryMutualRelationshipPatch()

    mutualRelationshipMap = affiliationAdjustment(mutualRelationshipMap)
    mutualRelationshipMap = relationAdjustment(mutualRelationshipMap, FRIENDSHIPS)
    mutualRelationshipMap = relationAdjustment(mutualRelationshipMap, ENMITIES)

    println("Saving country mutual relationships: " + mutualRelationshipMap)

    CountrySource.updateCountryMutualRelationships(mutualRelationshipMap)

    println("DONE")
  }

  def affiliationAdjustment(existingMap : mutable.Map[(String, String), Int]) : Map[(String, String), Int] = {
    println(s"affiliations: $AFFILIATIONS")
    AFFILIATIONS.foreach {
      case Affiliation(id, relationship, members) =>
        members.foreach { memberX =>
          if (CountrySource.loadCountryByCode(memberX).isDefined) {
            members.foreach { memberY =>
              if (memberX != memberY) {
                val shouldPatch = existingMap.get((memberX, memberY)) match {
                  case Some(existingValue) => existingValue < relationship
                  case None => true
                }
                if (shouldPatch) {
                  println(s"patching $memberX vs $memberY from $id with $relationship")
                  existingMap.put((memberX, memberY), relationship)
                } else {
                  println(s"Not patching $memberX vs $memberY from $id with $relationship as existing value is greater")
                }
              }
            }
          } else {
            println(s"Country code $memberX not found")
          }
        }
    }
    existingMap
  }

  def relationAdjustment(existingMap: mutable.Map[(String, String), Int], adjustmentMap: List[Relation] ): Map[(String, String), Int] = {
    import Direction._
    adjustmentMap.foreach {
      case Relation(id, direction, relationship, members) =>
        members.foreach { member =>
          if (CountrySource.loadCountryByCode(member).isDefined && member != id) {
            if(direction == Direction.TO){
              existingMap.put((member, id), relationship)
              println(s"$member -> $id with $relationship")
            } else if (direction == Direction.FROM) {
              existingMap.put((id, member), relationship)
              println(s"$id -> $member with $relationship")
            } else {
              existingMap.put((id, member), relationship)
              existingMap.put((member, id), relationship)
              println(s"$id <-> $member with $relationship")
            }
          } else {
            println(s"Country code $member not found | duplicate entry")
          }
        }
    }
    existingMap
  }

  /**
   * get from country-mutual-relationship.csv
   */
  def getCountryMutualRelationship() = {
    val nameToCode = CountrySource.loadAllCountries().map( country => (country.name, country.countryCode)).toMap
//    val linesIter = scala.io.Source.fromFile("country-mutual-relationship.csv").getLines()
//    val headerLine = linesIter.next()
//
//    val countryHeader = headerLine.split(',').filter(!_.isEmpty())
//
    val mutualRelationshipMap = Map[(String, String), Int]()
//
//    while (linesIter.hasNext) {
//      val tokens = linesIter.next().split(',').filter(!_.isEmpty())
//      //first token is the country name itself
//      val fromCountry = tokens(0)
//      for (i <- 1 until tokens.size) {
//        val relationship = tokens(i)
//        val strength = relationship.count( _ == '1') //just count the number of ones should be sufficient
//        val toCountry = countryHeader(i - 1)
//        //println(fromCountry + " " + toCountry + " " + strength)
//        if (strength > 0) {
//          if (nameToCode.contains(fromCountry) && nameToCode.contains(toCountry)) {
//            mutualRelationshipMap.put((nameToCode(fromCountry), nameToCode(toCountry)), strength)
//          }
//        }
//      }
//    }

    nameToCode.values.foreach { countryCode =>
      mutualRelationshipMap.put((countryCode, countryCode), 5) //country with itself is 5 HomeCountry
    }

    mutualRelationshipMap
  }

  /**
   * patch from country-mutual-relationship-patch.csv
   */
  def getCountryMutualRelationshipPatch() = {
    val linesIter = scala.io.Source.fromFile("country-mutual-relationship-patch.csv").getLines()
    val mutualRelationshipMap = Map[(String, String), Int]()
    
    while (linesIter.hasNext) {
      val tokens = linesIter.next().split(',')
      //first token is the country name itself
      val fromCountry = tokens(0)
      val toCountry = tokens(1)
      val strength = Integer.valueOf(tokens(2))
      mutualRelationshipMap.put((fromCountry, toCountry), strength)
      mutualRelationshipMap.put((toCountry, fromCountry), strength)
    }
    mutualRelationshipMap
  }
  case class Relation(id : String, direction : Direction.Value, relationship: Int, members : List[String])

  object Direction extends Enumeration {
    type Direction = Value
    val FROM, TO, BI = Value
  }

  case class Affiliation(id : String, relationship: Int, members : List[String])



}