package com.patson.init

import com.patson.{Authentication, DemandGenerator, Util}
import com.patson.data._
import com.patson.data.airplane._
import com.patson.init.GeoDataGenerator.calculateLongitudeBoundary
import com.patson.model._
import com.patson.model.airplane._

import java.util.Calendar
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Set}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Random


object GenericTransitGenerator {

  //larger airport goes first
  val TRANSIT_MANUAL_LINKS = Map(
    "HND" -> "NRT",
    "KIX" -> "KIX",
    "KOJ" -> "KMI",
    "KPO" -> "USN",
    "TAE" -> "USN",
    "TAE" -> "PUS",
    "AOG" -> "SHE",
    "PEK" -> "PKX",
    "PKX" -> "TSN",
    "PEK" -> "TSN",
    "PEK" -> "TVS",
    "WUX" -> "SHA",
    "WUX" -> "PVG",
    "WUX" -> "CZX",
    "YTY" -> "CZX",
    "WHA" -> "NKG",
    "SZX" -> "CAN",
    "JJN" -> "XMN",
    "CTU" -> "MIG",
    "MIG" -> "TFU",
    "CTU" -> "TFU",
    "YPB" -> "TFU",
    "YPB" -> "CTU",
    "KGT" -> "TFU",
    "KGT" -> "CTU",
    "KJH" -> "KWE",
    "AMD" -> "BDQ",
    "STV" -> "BDQ",
    "PNQ" -> "BOM",
    "SDW" -> "GOI",
    "PNY" -> "MAA",
    "TIR" -> "MAA",
    "CCJ" -> "COK",
    "CCJ" -> "CJB",
    "CJB" -> "COK",
    "IXP" -> "DHM",
    "LWN" -> "EVN",
    "IST" -> "SAW",
    "SAW" -> "YEI",
    "EWR" -> "TTN",
    "HPN" -> "EWR",
    "HVN" -> "HPN",
    "TTN" -> "PHL",
    "PHL" -> "ACY",
    "PHL" -> "LNS",
    "JFK" -> "ISP",
    "HTO" -> "ISP",
    "BOS" -> "PSM",
    "BOS" -> "MHT",
    "BOS" -> "ORH",
    "ORH" -> "MHT",
    "ORH" -> "BDL",
    "HVN" -> "BDL",
    "BOS" -> "PVD",
    "MIA" -> "PBI",
    "FLL" -> "PBI",
    "MCO" -> "DAB",
    "MCO" -> "LAL",
    "MCO" -> "VRB",
    "MCO" -> "MLB",
    "VRB" -> "MLB",
    "JAX" -> "GNV",
    "JAX" -> "UST",
    "DAB" -> "UST",
    "VPS" -> "ECP",
    "VPS" -> "PNS",
    "GPT" -> "MSY",
    "BTR" -> "MSY",
    "BTR" -> "LFT",
    "IAH" -> "BTP",
    "SAF" -> "ABQ",
    "ELP" -> "LRU",
    "SLC" -> "PVU",
    "MWA" -> "CGI",
    "MWA" -> "PAH",
    "LAX" -> "ONT",
    "LAX" -> "SBA",
    "SNA" -> "ONT",
    "SBD" -> "PSP",
    "MRY" -> "SJC",
    "SMF" -> "SCK",
    "OAK" -> "SCK",
    "SFO" -> "SCK",
    "ISP" -> "JFK",
    "LGA" -> "SWF",
    "CAK" -> "CLE",
    "FNT" -> "DTW",
    "TOL" -> "DTW",
    "LAF" -> "IND",
    "LAF" -> "ORD",
    "LAF" -> "MDW",
    "SBN" -> "ORD",
    "SBN" -> "MDW",
    "RFD" -> "ORD",
    "MKE" -> "ORD",
    "SWO" -> "TUL",
    "SWO" -> "OKC",
    "STL" -> "BLV",
    "SEA" -> "PAE",
    "PDX" -> "SLE",
    "SLE" -> "EUG",
    "KOA" -> "ITO",
    "YYZ" -> "YKF",
    "YYZ" -> "YHM",
    "YYZ" -> "YKF",
    "YVR" -> "YXX",
    "FAI" -> "CKX",
    "NLU" -> "TLC",
    "MEX" -> "TLC",
    "MEX" -> "PBC",
    "AAZ" -> "GUA",
    "GRU" -> "VCP",
    "CGH" -> "VCP",
    "LGW" -> "LTN",
    "STN" -> "LHR",
    "STN" -> "LGW",
    "LBA" -> "MAN",
    "NCL" -> "MME",
    "GLA" -> "EDI",
    "AMS" -> "EIN",
    "EIN" -> "MST",
    "RTM" -> "EIN",
    "OST" -> "BRU",
    "LGG" -> "BRU",
    "XCR" -> "CDG",
    "DOL" -> "CDG",
    "CDG" -> "BVA",
    "ORY" -> "BVA",
    "EGC" -> "BOD",
    "CMF" -> "LYS",
    "MRS" -> "TLN",
    "MRS" -> "FNI",
    "BZR" -> "PGF",
    "BCN" -> "REU",
    "BCN" -> "ILD",
    "BCN" -> "GRO",
    "SDR" -> "BIO",
    "VGO" -> "SCQ",
    "BRN" -> "ZRH",
    "CDT" -> "VLC",
    "RMU" -> "ALC",
    "GRX" -> "AGP",
    "OPO" -> "VSE",
    "DUS" -> "NRN",
    "DUS" -> "CGN",
    "DUS" -> "DTM",
    "CGN" -> "DTM",
    "FMO" -> "DTM",
    "PAD" -> "DTM",
    "PAD" -> "KSF",
    "HAM" -> "BRE",
    "BLL" -> "KRP",
    "AGH" -> "CPH",
    "OSL" -> "TRF",
    "ARN" -> "VST",
    "ARN" -> "NYO",
    "GVA" -> "BRN",
    "BSL" -> "BRN",
    "ZRH" -> "BSL",
    "RMI" -> "AOI",
    "BGY" -> "MXP",
    "PSA" -> "FLR",
    "BLQ" -> "FRL",
    "TPS" -> "PMO",
    "CTA" -> "CIY",
    "TZL" -> "SJJ",
    "OMO" -> "SJJ",
    "ZIA" -> "SVO",
    "DME" -> "SVO",
    "CSY" -> "KZN",
    "ULV" -> "KZN",
    "NBC" -> "KZN",
    "ULV" -> "KUF",
    "GSV" -> "BWO",
    "SVX" -> "CEK",
    "NJC" -> "SGC",
    "KGO" -> "SGC",
    "NJC" -> "KGP",
    "KEJ" -> "TOF",
    "BAX" -> "OVB",
    "MEL" -> "AVV",
    "PMR" -> "WLG"
  )
  val ISLANDS = List(
    "ZNZ", //africa
    "SKN", "SSJ", "BNN", "MOL", "BYR", "HGL", "BMK", "GWT", "BMR", "HAU", //europe
    "JAG", //dk / greenland
    "HZK", "GRY", //is
    "ANX", //no
    "NNR", //ie
    "EGH", "EOI", "FIE", "FOA", "LWK", "LSI", "ACI", "TRE", "BRR", "BEB", "SYY", "KOI", "ILY", "CAL", "ISC", "GCI", "JER", "GIB", "IOM", "EOI", //GB
    "BIC", "UVE", "TGL", "IDY", //fr
    "HGL", "BMK", "GWT", "BMR", //de
    "GRW", "CVU", "PXO", "SJZ", "TER", "PIX", //pt
    "VDE", "GMZ", "TFN", //es
    "OLB", "EBA", //it
    "KDL", "URE", //ee
    "AOK", "KSJ", "JMK", "JNX", "JSI", "JTR", "KIT", "LKS", "MLO", "SMI", "JIK", "KGS", "RHO", "LXS", "MJT", "JKH", "ZTH", "EFL", "SMI", //gr
    "ECN", //turkish CY
    "KUM", "TNE", "MYE", "MK1", "OIM", "HAC", "AO1", "SDS", "OIR", "RIS", "OKI", "TSJ", "FUJ", "KKX", "TKN", "OKE", "RNJ", "UEO", "OKA", "MMY", "TRA", "ISG", "OGN", "IKI", "MMD", "KTD", "OIM", //JP
    "KNH", "MZG", //TW
    "TBH", "TAG", "BCD", "IAO", "CGM", //ph
    "LBU", "BTH", "LGK", "TMC", "BMU", "TTE", "ARD", //my & id
    "KOS", //vn
    "NMF", "HRF", "KDM", "NAN", "MEE", "PTF", "ELC", "PMK", //oceania & AU
    "WLG", //nz (channel crossing)
    "HRF", "HDK", "PRI", //indian ocean
    "WRG", "MQC", "YBC", "YCD", "LAK", "YPN", "YZG", "YEV", //ca
    "MVY", "ACK", "FRD", "ESD", "MKK", "LNY", "BID", "AVX", "JNU", "HNH", "ISP", "HTO",  //us
    "CZM", //mx
    "MQS", "GST", "VQS", "CPX", "GBJ", "SBH", "SAB", "UNI", "STT", "EIS", "AXA", "PLS", "GDT", "GGT", "SVD", "HID", "APW", "PTT", "MNI", "TCB", "JIK", "ESD", "HHH", "SAQ", "TIQ", "HOR", "PIX", "KOI", "HTI", "BON", "CRU", "EUX",
    "SPR",
    "PMV" //ve
  )
  val ISOLATED_COUNTRIES = Array("FO", "BS", "KY", "TC", "VC", "GD", "DM", "AG", "MS", "BQ", "BL", "MF", "SX", "AI", "VI", "VG", "VC", "VU", "WF", "MU", "MV", "CC", "CK", "CV", "ST", "NP")

  def main(args : Array[String]) : Unit = {
    LinkSource.deleteLinksByCriteria(List(("transport_type", TransportType.GENERIC_TRANSIT.id)))
//    generateGenericTransit()
    Await.result(actorSystem.terminate(), Duration.Inf)
  }
  def generateGenericTransit(range : Int = 50) : Unit = {
    val airports = AirportSource.loadAllAirports(true).filter(_.population >= 500).filter(_.runwayLength >= 500).filter { airport => !ISOLATED_COUNTRIES.contains(airport.countryCode) }.sortBy { _.power }

    var counter = 0;
    var progressCount = 0;

    val processed = mutable.HashSet[(Int, Int)]()
    val countryRelationships = CountrySource.getCountryMutualRelationships()
    for (airport <- airports) {
      //calculate max and min longitude that we should kick off the calculation
      val boundaryLongitude = calculateLongitudeBoundary(airport.latitude, airport.longitude, range)
      val airportsInRange = scala.collection.mutable.ListBuffer[(Airport, Double)]()
      for (targetAirport <- airports) {
        if (!processed.contains((targetAirport.id, airport.id)) && //check the swap pairs are not processed already to avoid duplicates
          (TRANSIT_MANUAL_LINKS.get(airport.iata).contains(targetAirport.iata) || TRANSIT_MANUAL_LINKS.get(targetAirport.iata).contains(airport.iata))
        ) {
          val distance = Util.calculateDistance(airport.latitude, airport.longitude, targetAirport.latitude, targetAirport.longitude).toInt
          airportsInRange += Tuple2(targetAirport, distance)
        } else if (airport.id != targetAirport.id &&
            (!ISLANDS.contains(airport.iata) || !ISLANDS.contains(targetAirport.iata)) &&
            targetAirport.popMiddleIncome > 2500 &&
            !processed.contains((targetAirport.id, airport.id)) && //check the swap pairs are not processed already to avoid duplicates
            airport.longitude >= boundaryLongitude._1 && airport.longitude <= boundaryLongitude._2 &&
            countryRelationships.getOrElse((airport.countryCode, targetAirport.countryCode), 0) >= 2
        ) {
          val distance = Util.calculateDistance(airport.latitude, airport.longitude, targetAirport.latitude, targetAirport.longitude).toInt
          if (range >= distance) {
            airportsInRange += Tuple2(targetAirport, distance)
          }
        }
        processed.add((airport.id, targetAirport.id))
      }




      airportsInRange.foreach { case (targetAirport, distance) =>
        val isDomesticAirport = if(targetAirport.isDomesticAirport() || airport.isDomesticAirport()) 2 else 0
        val isGatewayAirport = if(targetAirport.isGateway() || airport.isGateway()) 3.5 else 0
        val multiplier = Math.min(airport.size, targetAirport.size) + isDomesticAirport + isGatewayAirport
        val capacity = (multiplier * 12000).toInt
        val genericTransit = GenericTransit(from = airport, to = targetAirport, distance = distance.toInt, capacity = LinkClassValues.getInstance(economy = capacity, business = (capacity * 0.3).toInt))
        LinkSource.saveLink(genericTransit)
        println(s"${airport.iata}, ${airport.countryCode}, ${targetAirport.iata}, ${targetAirport.countryCode}, $distance, $capacity")
      }

      val progressChunk = airports.size / 100
      counter += 1
      if (counter % progressChunk == 0) {
        progressCount += 1;
        print(".")
        if (progressCount % 10 == 0) {
          print(progressCount + "% ")
        }
      }
    }
  }
}