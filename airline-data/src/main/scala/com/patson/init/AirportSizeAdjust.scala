package com.patson.init

import com.patson.model._
import com.patson.data.AirportSource
import scala.collection.mutable.ListBuffer

object AirportSizeAdjust {
  //https://en.wikipedia.org/wiki/List_of_busiest_airports_by_passenger_traffic
  //orderd per 2024 stats top 10; 2023 elsewhere
  //level 10 = top 10
  //level 9 >= ~ 36m pax
  //level 8 >= ~ 20m pax
  //level 7 >= ~ 10m pax
  //level 6 >= ~ 2m pax
  //level 5 >= ~ 1m pax
  val sizeList = Map(
        //top 5
        "ATL" -> 10,
        "DXB" -> 10,
        "HND" -> 10,
        "LHR" -> 10,
        "DFW" -> 10,
        //level 9
        "DEN" -> 9,
        "CAN" -> 9,
        "IST" -> 9,
        "PVG" -> 9,
        "ORD" -> 9,
        "CDG" -> 9,
        "AMS" -> 9,
        "FRA" -> 9,
        "MAD" -> 9,
        "LAX" -> 9,
        "JFK" -> 9,
        "PEK" -> 9,
        "DEL" -> 8,//should be a 9 but it gets too big
        "ICN" -> 9,
        "SIN" -> 9,
        "SZX" -> 9,
        "CGK" -> 9,
        //level 8; minus some USA airports
        "BCN" -> 8,
        "FCO" -> 8,
        "MUC" -> 8,
        "DOH" -> 8,
        "JED" -> 8,
        "RUH" -> 8,
        "CAI" -> 8,
        "MEX" -> 8,
        "GRU" -> 8,
        "LAS" -> 8,
        "SEA" -> 8,
        "MIA" -> 8,
        "SFO" -> 8,
        "MCO" -> 7,
        "PHX" -> 8,
        "MEX" -> 8,
        "KUL" -> 8,
        "MNL" -> 8,
        "YYZ" -> 8,
        "TFU" -> 8,
        "CKG" -> 8,
        "KMG" -> 8,
        "XIY" -> 8,
        "GRU" -> 8,
        "HGH" -> 8,
        "SGN" -> 8,
        "HKG" -> 8,
        "TPE" -> 7,
        "BKK" -> 7,
        "MFM" -> 7,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Europe
        //2023, up to 10m pax, excluding "discount" airports
        "LGW" -> 7, //should be 8
        "SVO" -> 7,
        "LIS" -> 7,
        "DUB" -> 7,
        "ORY" -> 7,
        "VIE" -> 7,
        "ZRH" -> 7,
        "ATH" -> 7,
        "MAN" -> 7,
        "CPH" -> 7,
        "MXP" -> 7,
        "OSL" -> 7,
        "BER" -> 7,
        "AGP" -> 7,
        "BRU" -> 7,
        "DME" -> 7,
        "ARN" -> 7,
        "LED" -> 7,
        "DUS" -> 7,
        "WAW" -> 7,
        "ALC" -> 7,
        "HEL" -> 7,
        "BUD" -> 7,
        "NCE" -> 7,
        "OTP" -> 7,
        "EDI" -> 7,
        //keeping islands small so they don't spill over other islands
        "LPA" -> 5,
        "TFS" -> 5,
        "TFN" -> 5,
        "PMI" -> 5,
        "FUE" -> 4,
        "ACE" -> 4,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_South_America
        "LIM" -> 7,
        "SCL" -> 7,
        "CGH" -> 6,
        "BSB" -> 7,
        "GIG" -> 7,
        "MDE" -> 7,
        "AEP" -> 6,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_the_Middle_East
        "AUH" -> 7,
        "TLV" -> 7,
        "KWI" -> 7,
        "MCT" -> 7,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Africa
        "JNB" -> 7,
        "CPT" -> 7,
        "CMN" -> 7,
        "LOS" -> 7,
        "ADD" -> 7,
        //Oceania (giving some AU airports extra large range)
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Australia
        "SYD" -> 8,
        "MEL" -> 8,
        "PER" -> 7,
        "BNE" -> 7,
        "CBR" -> 6,
        "OOL" -> 5,
        "BNK" -> 3,
        "CNS" -> 6,
        "HBA" -> 5,
        "NTL" -> 4,
        "LST" -> 4,
        "TSV" -> 5,
        "BME" -> 5,
        "ASP" -> 4,
        "MKY" -> 3,
        "MCY" -> 4,
        "HTI" -> 3,
        "AVV" -> 3,
        //NZ
        "AKL" -> 7,
        "ZQN" -> 4,
        "WLG" -> 4,
        "XCH" -> 3,
        "NLK" -> 3,
        "HLZ" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Albania
        //algeria
        "TMR" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Angola
        "MSZ" -> 4,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Argentina
        "MDZ" -> 5,
        "BRC" -> 5,
        "SLA" -> 5,
        "TUC" -> 4,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Austria
        "SZG" -> 5,
        "INN" -> 4,
        "GRZ" -> 4,
        //AQ
        "TNM" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Bangladesh
        "CGP" -> 5,
        "ZYL" -> 5,
        //belgium
        "LGG" -> 3,
        "ANR" -> 3,
        "OST" -> 3,
        //bhutan
        "PBH" -> 4,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Burkina_Faso
        "BOY" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Benin
        //https://en.wikipedia.org/wiki/List_of_airports_in_Bolivia
        "LPB" -> 6,
        "VVI" -> 5,
        "SRE" -> 3,
        "TJA" -> 3,
        //bosnia
        "SJJ" -> 4,
        "BNX" -> 3,
        "OMO" -> 3,
        //botswana
        "MUB" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_the_Democratic_Republic_of_the_Congo
        "BDT" -> 3,
        "MJM" -> 3,
        "GOM" -> 4,
        //https://en.wikipedia.org/wiki/List_of_airports_in_the_Republic_of_the_Congo
        //bermuda
        "BDA" -> 6,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Brazil
        "SDU" -> 6,
        "CNF" -> 6,
        "REC" -> 6,
        "VCP" -> 6,
        "POA" -> 5,
        "SSA" -> 6,
        "FOR" -> 5,
        "CWB" -> 5,
        "BEL" -> 5,
        "GYN" -> 5,
        "MCZ" -> 4,
        "BPS" -> 4,
        "NVT" -> 4,
        "IGU" -> 4,
        "UNA" -> 3,
        "CFB" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Cameroon
        "DLA" -> 4,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Canada
        "YVR" -> 7,
        "YUL" -> 7,
        "YYC" -> 7,
        "YTZ" -> 4, //toronto city
        "YHM" -> 4,
        "YLW" -> 5, //Kelowna
        "YQT" -> 5, //Thunder Bay
        "YYJ" -> 5, //Victoria
        "YXE" -> 5, //Saskatoon
        "YQR" -> 4, //Regina
        "YDF" -> 4, //Deer Lake
        "YYT" -> 5, //St. John's
        "YXY" -> 4,
        "YZF" -> 4,
        "YXS" -> 4,
        "YCD" -> 4,
        //downgrades
        "YYR" -> 4,
        "YQQ" -> 4,
        "YVO" -> 3,
        "YBG" -> 3,
        "YCH" -> 2,
        "YJT" -> 2,
        "SLW" -> 2,
        //Cayman
        "CYB" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Chad
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Chile
        "ANF" -> 5,
        "CJC" -> 5,
        "CCP" -> 5,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_China
        //listing 2022 order + PVG
        "CTU" -> 6,
        "SHA" -> 7,
        "WUH" -> 7,
        "CSX" -> 7,
        "NKG" -> 7,
        "XMN" -> 6,
        "URC" -> 6,
        "MIG" -> 4,
        "PZI" -> 3,
        //upscaling oddly small airports
        "DNH" -> 4,
        "LUM" -> 5,
        "JIU" -> 3,
        "AVA" -> 3,
        "GOQ" -> 3,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Colombia
        "BOG" -> 8,
        "CLO" -> 6,
        "CTG" -> 6,
        "BAQ" -> 5,
        "SMR" -> 4,
        "PEI" -> 5,
        "FLA" -> 3,
        //congo
        "NLA" -> 4,
        //cote d'ivoire
        "BYK" -> 4,
        //costa-rico
        "SJO" -> 5,
        //cuba
        "CYO" -> 4,
        //denmark
        "AAL" -> 5,
        "KRP" -> 3,
        "FAE" -> 4, //Faroe Islands
        //dominican-republic
        "JBQ" -> 3,
        //east-timor
        "BIK" -> 4,
        //ecuador
        "GYE" -> 5,
        //egypt
        "SPX" -> 3,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_France
        "NTE" -> 6,
        "BES" -> 4,
        "GNB" -> 3,
        "XCR" -> 3,
        "TLN" -> 3,
        "LTT" -> 2,
        "FSP" -> 3,
        "PTP" -> 5,
        "WLS" -> 3,
        "AJA" -> 4,
        //ethiopia
        "MQX" -> 3,
        //eritrea
        "MSW" -> 1,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Germany
        "HAM" -> 7,
        "NUE" -> 6,
        "DTM" -> 5,
        "FMM" -> 4,
        "LEJ" -> 5,
        "BRE" -> 4,
        "LBC" -> 3,
        "MHG" -> 3,
        //finland
        "RVN" -> 4,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Greece
        "CFU" -> 5,
        "JMK" -> 3,
        "JSI" -> 3,
        "JTR" -> 3,
        "SMI" -> 3,
        "JIK" -> 2,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Guinea
        //https://en.wikipedia.org/wiki/List_of_airports_in_Guatemala
        //https://en.wikipedia.org/wiki/List_of_airports_in_Honduras
        "TGU" -> 4,
        "SAP" -> 5,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Haiti
        "CAP" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Jamaica
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Japan
        "FUK" -> 7,
        "CTS" -> 7,
        "OKA" -> 7,
        "NRT" -> 7,
        "KIX" -> 7,
        "ITM" -> 6,
        "KIX" -> 6,
        "KOJ" -> 5,
        "OKD" -> 4,
        "MSJ" -> 3,
        "HSG" -> 3,
        "TKS" -> 3,
        "IWJ" -> 3,
        "SDS" -> 3,
        "NKM" -> 2,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Kazakhstan
        "PLX" -> 3,
        "AKX" -> 4,
        //kenya
        "EDL" -> 4,
        "KIS" -> 4,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Kyrgyzstan
        "IKU" -> 2,
        //https://en.wikipedia.org/wiki/List_of_airports_in_North_Korea
        "FNJ" -> 4,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_India
        "BOM" -> 8,
        "BLR" -> 7,
        "HYD" -> 7,
        "MAA" -> 7,
        "CCU" -> 7,
        "AMD" -> 6,
        "PNQ" -> 6,
        "LKO" -> 6,
        "GAU" -> 6,
        "JAI" -> 6,
        "SXR" -> 6,
        "IXC" -> 6,
        "BBI" -> 6,
        "PAT" -> 5,
        "CCJ" -> 5,
        "IDR" -> 5,
        "VTZ" -> 5,
        "DED" -> 5,
        "IXR" -> 5,
        "GOX" -> 5,
        "IXB" -> 5,
        "TIR" -> 4,
        "AYJ" -> 4,
        "VNS" -> 4,
        "DHM" -> 3,
        "BHO" -> 4,
        "RPR" -> 4,
        "RDP" -> 4, //upscaling to cut CCU
        "AGR" -> 5, //upscaling to cut DEL
        "HDO" -> 3,
        "PGH" -> 3,
        "HSS" -> 3,
        "NAG" -> 3,
        "KNU" -> 2,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Indonesia
        "HLP" -> 4,
        "DPS" -> 7,
        "SUB" -> 6,
        "UPG" -> 6,
        "JOG" -> 6,
        "BTH" -> 6,
        "PLM" -> 6,
        "SRG" -> 6,
        "BDO" -> 6,
        "PNK" -> 6,
        "LOP" -> 6,
        "PKU" -> 6,
        "PDG" -> 6,
        "MDC" -> 5,
        "BIK" -> 3,
        "KDI" -> 3,
        "KOE" -> 5,
        "DJB" -> 4,
        "TTE" -> 4,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Iran
        "DEF" -> 3,
        "KSH" -> 4,
        "HDM" -> 3,
        "OMH" -> 3,
        "BXR" -> 3,
        "ZBR" -> 3,
        "AZD" -> 3,
        //iraq
        "EBL" -> 5,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Italy
        "NAP" -> 6,
        "BGY" -> 5,
        "CTA" -> 5,
        "PMO" -> 5,
        "BLQ" -> 5,
        "BRI" -> 5,
        "CAG" -> 5,
        "OLB" -> 5,
        "FLR" -> 5,
        "SUF" -> 5,
        "GOA" -> 5,
        "BZO" -> 3,
        "QSR" -> 3,
        //ireland
        "ORK" -> 5,
        "SNN" -> 5,
        //israel
        "HFA" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Ivory_Coast
        "ABJ" -> 5,
        //laos
        "VTE" -> 5,
        "PKZ" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Liberia
        //https://en.wikipedia.org/wiki/List_of_airports_in_Lithuania
        "KUN" -> 3,
        "VNO" -> 5,
        //Libya
        "GHT" -> 2,
        "AKF" -> 2,
        "SEB" -> 3,
        //jamica
        "MBJ" -> 4,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Madagascar
        "ERS" -> 2,
        //Mayotte
        "DZA" -> 4,
        //Marshal Islands
        "KWA" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_the_Republic_of_Macedonia
        //https://en.wikipedia.org/wiki/List_of_airports_in_Mali
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Mexico
        "CUN" -> 7,
        "GDL" -> 7,
        "MTY" -> 6,
        "TIJ" -> 6,
        "BJX" -> 5,
        "SJD" -> 5,
        "CJS" -> 5,
        "CUL" -> 5,
        "HMO" -> 5,
        "QRO" -> 5,
        "OAX" -> 5,
        "LAP" -> 4,
        "CUU" -> 4,
        "HUX" -> 4,
        "TGZ" -> 4,
        "TQO" -> 4,
        //downgrade
        "PBC" -> 3,
        "TLC" -> 3,
        "MLM" -> 4,
        "CVJ" -> 2,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Mongolia
        //https://en.wikipedia.org/wiki/List_of_airports_in_Mauritania
        //https://en.wikipedia.org/wiki/List_of_airports_in_Malawi
        //https://en.wikipedia.org/wiki/List_of_airports_in_Moldova
        //morocco
        "ERH" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Mozambique
        "INH" -> 3,
        //namibia
        "WVB" -> 3,
        "OMD" -> 3,
        "MPA" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Niger
        //https://en.wikipedia.org/wiki/List_of_airports_in_Nicaragua
        //https://en.wikipedia.org/wiki/List_of_airports_in_Nepal
        //norway
        "BGO" -> 6,
        "TRD" -> 6,
        "SVG" -> 6,
        "TOS" -> 5,
        "TRF" -> 4,
        "AES" -> 4,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Pakistan
        "SKT" -> 3,
        "UET" -> 3,
        "PEW" -> 4,
        //panama
        "BLB" -> 3,
        "RIH" -> 3,
        "PYC" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Paraguay
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_the_Philippines
        "DVO" -> 6,
        "MPH" -> 6,
        "DRP" -> 4,
        "GES" -> 3,
        "DRP" -> 3,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Poland
        "KRK" -> 6,
        "GDN" -> 6,
        "KTW" -> 6,
        "WRO" -> 5,
        "POZ" -> 5,
        "WMI" -> 4,
        //portugal
        "CAT" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Rwanda
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Russia
        "KZN" -> 6,
        "AER" -> 6,
        "SVX" -> 6,
        "KRR" -> 6,
        "KGD" -> 5,
        "PYJ" -> 3,
        "DYR" -> 3,
        "NER" -> 3,
        "NJC" -> 4,
        "KEJ" -> 4,
        "PKC" -> 4,
        "KGD" -> 4,
        "PWE" -> 3,
        "ESL" -> 2,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Saudi_Arabia
        "EJH" -> 3,
        "ABT" -> 3,
        "EJH" -> 3,
        "WAE" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Sierra_Leone
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Spain
        "VLC" -> 6,
        "SVQ" -> 6,
        "BIO" -> 6,
        "VIT" -> 3,
        "EAS" -> 3,
        "LEU" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Somalia
        "HGA" -> 3,
        "MGQ" -> 3,
        "GGR" -> 3,
        "BSA" -> 2,
        //https://en.wikipedia.org/wiki/List_of_South_African_airports_by_passenger_movements
        "DUR" -> 6,
        "HLA" -> 4,
        "PLZ" -> 5,
        "ELS" -> 5,
        "PTG" -> 4,
        "GRJ" -> 4,
        "BFN" -> 3,
        "HDS" -> 3,
        "MQP" -> 3,
        "PTG" -> 3,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_South_Korea
        "CJU" -> 6,
        "PUS" -> 7,
        "USN" -> 3,
        "RSU" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_El_Salvador
        //sri lanka
        "JAF" -> 3,
        //sweden
        "MMX" -> 5,
        "NYO" -> 4,
        "ORB" -> 3,
        "AGH" -> 3,
        "BMA" -> 5,
        //switzerland
        "BRN" -> 3,
        "LUG" -> 3,
        "ACH" -> 3,
        //Tawain
        "RMQ" -> 5,
        "TNN" -> 3,
        "CYI" -> 3,
        "HUN" -> 2,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Tajikistan
        //https://en.wikipedia.org/wiki/List_of_airports_in_Tanzania
        "BKZ" -> 2,
        "TKQ" -> 2,
        "ARK" -> 2,
        "MWZ" -> 3,
        "JRO" -> 4,
        "DAR" -> 5,
        //Taiwan
        "PIF" -> 1,
        "HCN" -> 2,
        "TNN" -> 3,
        //trinidad tobago
        "TAB" -> 4,
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Turkey
        "AYT" -> 7,
        "SAW" -> 5,
        "ESB" -> 6,
        "ADB" -> 6,
        "DLM" -> 5,
        "ADA" -> 5,
        "TZX" -> 5,
        "GZT" -> 5,
        "KYA" -> 4,
        "MLX" -> 4,
        "MSR" -> 4,
        "BAL" -> 4,
        "KSY" -> 3,
        "TEQ" -> 2,
        "YEI" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Turkmenistan
        "CRZ" -> 4,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Uganda
        //https://en.wikipedia.org/wiki/List_of_airports_in_Uruguay
        "PDP" -> 4,
        //https://en.wikipedia.org/wiki/List_of_busiest_airports_in_the_United_Kingdom
        "BRS" -> 6,
        "GLA" -> 5,
        "BFS" -> 5,
        "BHX" -> 5,
        "LPL" -> 4,
        "DSA" -> 3,
        "LCY" -> 4,
        "BHD" -> 4,
        "SOU" -> 3,
        "LDY" -> 3,
        "JER" -> 2,
        "BHD" -> 3,
        "ABZ" -> 3,
        "ASI" -> 3, //Ascension Island
        "HLE" -> 3, //St Helena
        //https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_the_United_States
        //up to 10m pax
        "EWR" -> 7, //should be 8
        "IAH" -> 7, //should be 8
        "BOS" -> 7,
        "CLT" -> 7,
        "FLL" -> 7,
        "MSP" -> 7,
        "DTW" -> 7,
        "PHL" -> 7,
        "SLC" -> 7,
        "DCA" -> 6,
        "SAN" -> 7,
        "BWI" -> 7,
        "TPA" -> 7,
        "AUS" -> 7,
        "IAD" -> 7,
        "BNA" -> 7,
        //metro NYC
        "LGA" -> 6,
        "SWF" -> 3,
        "ACY" -> 3,
        "HPN" -> 4,
        "HVN" -> 3,
        "BDL" -> 4,
        "ISP" -> 4,
        "LNS" -> 3,
        "TTN" -> 3,
        //metro LA
        "BUR" -> 4,
        "SNA" -> 4,
        "ONT" -> 3,
        "LGB" -> 3,
        "SBD" -> 3,
        "PMD" -> 2,
        "BFL" -> 3,
        "SBA" -> 4,
        "PSP" -> 6,
        //metro Chicago
        "MDW" -> 6,
        "RFD" -> 2,
        "BMI" -> 3,
        "MLI" -> 4,
        "MSN" -> 5,
        "LAF" -> 3,
        //metro SF
        "STS" -> 3,
        "SMF" -> 6,
        "MHR" -> 2,
        "SCK" -> 3,
        //Hawaii
        "HNL" -> 6,
        "OGG" -> 4,
        "KOA" -> 4,
        "LIH" -> 4,
        //downgrade secondary USA airports
        "BFI" -> 3,
        "LCK" -> 3,
        "BLV" -> 3,
        "GRK" -> 2,
        "TYR" -> 2,
        "PIA" -> 3,
        "FWA" -> 4,
        "OGD" -> 3,
        "PIH" -> 3,
        "PSM" -> 3,
        "LRU" -> 2,
        "USA" -> 3,
        "SWO" -> 2,
        "BKG" -> 2,
        "OWB" -> 2,
        //upgrade established city airports
        "SAT" -> 6,
        "OKC" -> 6,
        "DSM" -> 5,
        "ABQ" -> 6,
        "MKE" -> 6,
        "CLE" -> 6,
        "BUF" -> 6,
        "BTV" -> 4,
        "PBG" -> 4,
        "ROC" -> 5,
        "ALB" -> 4,
        "CHS" -> 5,
        "SAV" -> 5,
        "LIT" -> 4,
        "ORF" -> 5,
        "AVL" -> 4,
        //alaska
        "FAI" -> 5,
        "JNU" -> 4,
        "CDB" -> 3,
        "GAL" -> 2,
        "SXP" -> 2,
        //washington
        "BLI" -> 4,
        "YKM" -> 2,
        "PUW" -> 2,
        //oregon
        "SLE" -> 3,
        "RDM" -> 4,
        //montana
        "BZN" -> 5,
        "BIL" -> 5,
        "GTF" -> 4,
        "MSO" -> 4,
        //dakotas
        "FAR" -> 4,
        "DLH" -> 4,
        "FSD" -> 4,
        //colorado
        "COS" -> 4,
        "PUB" -> 3,
        "MTJ" -> 3,
        "GUC" -> 3,
        //utah
        "CNY" -> 3,
        //idaho
        "IDA" -> 4,
        //airzona
        "PRC" -> 2,
        "GCN" -> 2,
        "YUM" -> 3,
        "AZA" -> 4,
        //texas
        "DAL" -> 5,
        "HOB" -> 5,
        "SPS" -> 3,
        "GGG" -> 2,
        "AMA" -> 4,
        "LBB" -> 4,
        "ELP" -> 6,
        //florida
        "PGD" -> 4,
        "MEI" -> 2,
        "DAB" -> 4,
        "SFB" -> 3,
        "MLB" -> 3,
        "LAL" -> 3,
        "OPF" -> 1,
        //NE
        "ORH" -> 3,
        "MVY" -> 2,
        //downgrade more
        "DUL" -> 4,
        "BGR" -> 5,
        "MDT" -> 4,
        "LMT" -> 3,
        "RKS" -> 3,
        "SLN" -> 3,
        "LNK" -> 3,
        "TOL" -> 4,
        "CPR" -> 3,
        "ROW" -> 3,
        "MQT" -> 3,
        "FHU" -> 2,
        "GLH" -> 2,
        "GTR" -> 2,
        "DHN" -> 2,
        "BQK" -> 2,
        "HNS" -> 1,
        //puerto-rico
        "SIG" -> 2,
        "PSE" -> 3,
        "CPX" -> 2,
        "VQS" -> 2,
        //usvi
        "STT" -> 5,
        //Uzbekistan
        "NVI" -> 4,
        "UGC" -> 4,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Vietnam
        "VDO" -> 4,
        "HPH" -> 4,
        "DLI" -> 4,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Venezuela
        //https://en.wikipedia.org/wiki/List_of_airports_in_Kosovo
        //https://en.wikipedia.org/wiki/List_of_airports_in_Yemen
        "SCT" -> 3,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Zambia
        "LVI" -> 4,
        //https://en.wikipedia.org/wiki/List_of_airports_in_Zimbabwe
        "VFA" -> 4,
        "BUQ" -> 4,
      )
      
  
    
}