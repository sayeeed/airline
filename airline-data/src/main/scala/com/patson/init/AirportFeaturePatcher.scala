package com.patson.init

import com.patson.model._
import com.patson.data.AirportSource
import com.patson.data.DestinationSource

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object AirportFeaturePatcher extends App {

  import AirportFeatureType._

  lazy val featureList = Map(

    INTERNATIONAL_HUB -> Map[String, Int](
      /**
       * international vacation destinations
       */
"IST" -> 70, //Istanbul
"BKK" -> 67, //Bangkok
"HER" -> 67, //Heraklion
"CDG" -> 66, //Paris
"JED" -> 65, //Jeddah
"DAD" -> 64, //Da Nang
"DPS" -> 64, //Denpasar-Bali Island
"USM" -> 63, //Na Thon (Ko Samui Island)
"DXB" -> 63, //Dubai
"HKT" -> 62, //Phuket
"UTP" -> 62, //Rayong
"CNX" -> 61, //Chiang Mai
"PQC" -> 60, //Phu Quoc Island
"AYT" -> 59, //Antalya
"PNH" -> 53, //Phnom Penh
"LHR" -> 51, //London
"HKG" -> 50, //Hong Kong
"KUL" -> 50, //Kuala Lumpur
"PUJ" -> 49, //Punta Cana
"CXR" -> 49, //Nha Trang
"KBV" -> 49, //Krabi
"HRG" -> 48, //Hurghada
"NRT" -> 48, //Tokyo / Narita
"FCO" -> 47, //Rome
"BCN" -> 46, //Barcelona
"CUZ" -> 46, //Cusco
"DMK" -> 45, //Bangkok
"RHO" -> 45, //Rodes Island
"SYD" -> 44, //Sydney Australia
"MLE" -> 43, //Malé Maldives
"CUN" -> 42, //Cancún
"RAK" -> 42, //Marrakech
"JFK" -> 40, //New York
"VCE" -> 40, //Venice
"MIA" -> 40, //Miami
"MBJ" -> 39, //Montego Bay
"LGK" -> 38, //Langkawi
"ATH" -> 38, //Athens
"HND" -> 38, //Tokyo / Haneda
"NCE" -> 38, //Nice
"SSH" -> 37, //Sharm el-Sheikh
"KIX" -> 37, //Osaka
"NAN" -> 37, //Nadi
"GRU" -> 36, //São Paulo
"CMB" -> 36, //Colombo
"CZM" -> 36, //Cozumel
"RMF" -> 36, //Marsa Alam
"LAX" -> 36, //Los Angeles
"LGW" -> 36, //London United Kingdom
"VIE" -> 35, //Vienna
"CPT" -> 35, //Cape Town
"GIG" -> 35, //Rio De Janeiro
"SCL" -> 34, //Santiago
"HAV" -> 34, //Havana
"CAI" -> 34, //Cairo Egypt
"ICN" -> 34, //Seoul
"MAD" -> 34, //Madrid
"BER" -> 33, //Berlin
"AMS" -> 33, //Amsterdam
"MEL" -> 32, //Melbourne
"HNL" -> 32, //Honolulu
"LIS" -> 32, //Lisbon
"BJV" -> 32, //Bodrum
"SIN" -> 32, //Singapore
"ASR" -> 31, //Kayseri
"YYZ" -> 31, //Toronto Canada
"AGA" -> 31, //Agadir
"CTS" -> 30, //Chitose / Tomakomai
"PMI" -> 30, //Palma De Mallorca
"EWR" -> 30, //New York City USA
"SPX" -> 30, //Cairo
"CEB" -> 29, //Lapu-Lapu City
"SAI" -> 29, //Siem Reap
"MEX" -> 29, //Mexico City
"AGP" -> 28, //Málaga
"AEP" -> 28, //Buenos Aires
"MPH" -> 28, //Malay
"PEN" -> 28, //Penang
"DJE" -> 28, //Djerba
"PPT" -> 28, //Papeete
"EZE" -> 28, //Buenos Aires
"MRU" -> 28, //Port Louis
"MUC" -> 27, //Munich
"LPB" -> 27, //La Paz / El Alto
"CMN" -> 27, //Casablanca
"BOM" -> 27, //Mumbai
"BKI" -> 26, //Kota Kinabalu
"FUE" -> 26, //Fuerteventura Island
"KGS" -> 26, //Kos Island
"TLV" -> 26, //Tel Aviv
"PRG" -> 26, //Prague
"KEF" -> 26, //Reykjavík
"DUB" -> 25, //Dublin Ireland
"LAS" -> 25, //Las Vegas
"CHQ" -> 24, //Heraklion
"LAP" -> 24, //La Paz
"VRA" -> 24, //Varadero
"BUD" -> 24, //Budapest
"NOU" -> 24, //Nouméa
"POP" -> 24, //Puerto Plata Dominican Republic
"TFS" -> 23, //Tenerife Island
"CPH" -> 23, //Copenhagen
"GUM" -> 23, //Hagåtña Guam International Airport
"KTM" -> 23, //Kathmandu
"BAH" -> 22, //Manama
"ADB" -> 22, //Izmir
"PEK" -> 22, //Beijing
"TPE" -> 22,
"BNE" -> 21, //Brisbane
"TFN" -> 21, //Tenerife Island
"SGN" -> 21,
"GOX" -> 21, //Goa IN
"NAP" -> 21, //Nápoli
"PDL" -> 21, //Azores
"GZP" -> 21, //Gazipaşa
"SEZ" -> 21, //Mahe Island
"SFO" -> 21, //San Francisco
"DRW" -> 21, //Darwin
"MXP" -> 20, //Milan
"LPA" -> 20, //Gran Canaria Island
"ARN" -> 20, //Stockholm
"CIA" -> 20, //Ostia Antica Italy
"GOI" -> 20, //Vasco da Gama
"LPQ" -> 20, //Luang Phabang
"SLL" -> 20, //Salalah
"OSL" -> 20, //Oslo
"ORD" -> 20, //Chicago
"LCA" -> 19, //Larnarca
"IBZ" -> 19, //Ibiza
"LIM" -> 19,
"ZRH" -> 19, //Zurich
"TUN" -> 19, //Tunis
"OGG" -> 18, //Kahului
"CTG" -> 18, //Cartagena
"POA" -> 18, //Porto Alegre
"YVR" -> 18, //Vancouver
"CNS" -> 18, //Cairns
"AKL" -> 18, //Auckland
"FNC" -> 18, //Funchal
"ZQN" -> 18, //Queenstown
"SVO" -> 18, //Moscow
"BVC" -> 18, //Rabil
"EDI" -> 18, //Edinburgh
"MCT" -> 18, //Muscat
"MIR" -> 18, //Monastir
"MVD" -> 18, //Montevideo
"PVG" -> 18, //Shanghai
"TIA" -> 18, //Triana
"GVA" -> 18,
"MCO" -> 18, //Orlando
"VAR" -> 17, //Varna
"SJU" -> 17, //San Juan
"SSA" -> 17, //Salvador
"JTR" -> 17, //Santorini Island
"IKA" -> 17, //Tehran
"MNL" -> 17,
"NBE" -> 17, //Enfidha
"DLM" -> 17, //Dalaman
"PFO" -> 17, //Paphos
"PSA" -> 16, //Pisa
"CFU" -> 16, //Kerkyra Island
"AUA" -> 16, //Oranjestad
"BGI" -> 16, //Bridgetown
"BGY" -> 16, //Milan
"BSL" -> 16, //Mulhouse French/Swiss Alps
"DOH" -> 16,
"HUI" -> 16, //Hue Phu Bai VN
"SJO" -> 16, //San Jose
"WAW" -> 16, //Warsaw
"PTY" -> 16,
"CJU" -> 15, //Jeju City
"AER" -> 15, //Sochi
"CAG" -> 15, //Cagliari
"RUN" -> 15, //St Denis
"FLR" -> 15, //Firenze
"ACE" -> 15, //Lanzarote Island
"CUR" -> 15, //Willemstad
"KIN" -> 15, //Kingston
"IAD" -> 15, //Washington
"GPS" -> 15, //Baltra Galapagos
"HEL" -> 15, //Helsinki
"OPO" -> 15,
"OKA" -> 14, //Naha
"HAN" -> 14, //Hanoi
"SJD" -> 14, //San José del Cabo
"PVR" -> 14, //Puerto Vallarta
"PER" -> 14, //Perth
"VKO" -> 14, //Moscow
"KRK" -> 14, //Kraków
"COK" -> 14, //Kochi
"JRO" -> 14, //Arusha
"VCS" -> 14, //Con Dao VN
"ATL" -> 14,
"DME" -> 14, //Moscow
"YQB" -> 14, //Quebec
"BEY" -> 14,
"PQC" -> 14,
"MED" -> 13, //Medina
"LED" -> 13, //St. Petersburg
"TIV" -> 13, //Tivat
"SID" -> 13, //Espargos
"RTB" -> 13, //Roatan Island
"BOD" -> 13, //prehistoric caves France
"EVN" -> 13,
"KOS" -> 12, //Sihanukville
"SHJ" -> 12, //Sharjah AE
"CTA" -> 12, //Catania
"PTP" -> 12, //Pointe-Ã -Pitre
"SKD" -> 12, //Samarkand
"STT" -> 12, //Charlotte Amalie
"NAS" -> 12, //Nassau
"SXM" -> 12, //Saint Martin
"JMK" -> 12, //Mykonos Island
"COV" -> 12, //Mersin TR
"NLU" -> 12, //Mexico City
"QSR" -> 12, //Amalfi coast
"PLZ" -> 12, //Addo Elephant National Park South Africa
"HDS" -> 12, //Kruger National Park South Africa
"PPS" -> 12, //Puerto Princesa City
"EBB" -> 12, //Kampala
"LBJ" -> 12, //Komodo National Park Indonesia
"MAO" -> 12, //Manaus
"PDP" -> 12, //Punta del Este
"RAI" -> 12, //Praia
"TBS" -> 12, //Tbilisi
"TNG" -> 12, //Tangiers
"TQO" -> 12, //Tulum
"VTE" -> 12, //Luang Prabang Laos
"BTH" -> 11, //Batam Island
"PMO" -> 11, //Palermo
"DBV" -> 11, //Dubrovnik
"VFA" -> 11, //Victoria Falls
"LIR" -> 11, //Liberia Costa Rica
"AMM" -> 11, //Amman
"ZTH" -> 11, //Zakynthos Island
"GAN" -> 11, //Maldives
"LXA" -> 11, //Lhasa
"VDO" -> 11, //Van Don VN
"LRM" -> 11, //La Romana DR
"TGD" -> 11,
"MRS" -> 11, //Marseille
"SAW" -> 10, //Istanbul
"FAO" -> 10, //Faro
"YUL" -> 10, //Montreal
"GYD" -> 10, //Baku
"MAH" -> 10, //Menorca Island
"MFM" -> 10, //Macau
"SPU" -> 10, //Split
"HUX" -> 10, //Huatulco
"BWN" -> 10, //Bandar Seri Begawan
"JNU" -> 10, //Juneau
"TNM" -> 10, //AQ
"AUH" -> 10,
"CGK" -> 10,
"DEN" -> 10,
"REC" -> 9, //Recife
"LVI" -> 9, //Livingstone
"NBO" -> 9, //Nairobi
"JAI" -> 9, //Jaipur
"JNB" -> 9, //Johannesburg
"FLG" -> 9, //Flagstaff Grand Canyon
"ZNZ" -> 9, //Zanzibar
"YZF" -> 9, //Yellowknife
"ANC" -> 9, //Anchorage
"GUA" -> 9, //Tikal Guatemala
"CCJ" -> 9, //Calicut
"TOS" -> 8, //Tromsø
"IGU" -> 8, //Foz Do IguaÃ§u
"CJC" -> 8, //Calama
"UVF" -> 8, //Vieux Fort
"MUB" -> 8, //Maun
"STX" -> 8, //Christiansted
"SPC" -> 8,
"SZG" -> 8, //Salzburg Austrian Alps
"CCC" -> 8, //Cayo Coco
"SEA" -> 8,
"GND" -> 8,
"FLW" -> 8, //Azores Flores
"SMA" -> 8, //Azores
"UPN" -> 8, //Kgalagadi Transfrontier Park South Africa/Botswana
"VDE" -> 8, //Canary Islands
"TAO" -> 8, //Qingdao
"NJF" -> 8, //Shia pilgirms
"MSY" -> 7, //New Orleans
"IGR" -> 7, //Puerto Iguazu
"BRI" -> 7, //Bari
"BIO" -> 7, //Bilbao
"MQP" -> 7, //Mpumalanga
"TER" -> 7, //Azores Lajes
"SZG" -> 7, //Berchtesgaden National Park Germany
"KLO" -> 7, //Boracay
"XIY" -> 7, //Xi'an
"LIF" -> 7, //Lifou
"LJU" -> 7, //Triglav National Park Slovenia
"GDT" -> 7, //Cockburn Town
"BZE" -> 7, //Chiquibul National Park Belize
"SLC" -> 7, //Salt Lake City
"DLI" -> 7,
"FDF" -> 6, //Fort-de-France
"GMZ" -> 6, //Canary Islands
"GCN" -> 6, //Grand Canyon
"CGB" -> 6, //Cuiabá Ecotourism
"CYO" -> 6, //Cayo Largo del Sur Cuba
"USH" -> 6, //Ushuahia
"BTS" -> 6, //Devin Castle Slovakia
"FPO" -> 6, //Bahamas
"LXR" -> 6, //Luxor
"PNT" -> 6, //Torres del Paine National Park Chile
"SJZ" -> 6, //Azores São Jorge
"WVB" -> 6,
"FAE" -> 6, //Faroe Islands
"ALG" -> 6, //Algiers
"KLX" -> 6,
"ASP" -> 5, //Alice Springs
"XIY" -> 5, //Terracotta Army China
"AYQ" -> 5, //Ayers Rock
"TRV" -> 5, //Thiruvananthapuram
"PTF" -> 5, //Mamanuca Islands
"HOG" -> 5, //CU
"FPO" -> 5,
"HAL" -> 5,
"HOR" -> 5, //Azores Horta
"MFU" -> 5,
"PUQ" -> 5, //Punta Arenas
"SEU" -> 5,
"SKB" -> 5,
"TAB" -> 5,
"ZSA" -> 5,
"BRC" -> 4, //San Carlos de Bariloche
"SMR" -> 4, //Santa Marta
"ANU" -> 4, //St. John's
"BOJ" -> 4, //Burgas
"BON" -> 4, //Kralendijk Bonaire
"MCZ" -> 4,
"FTE" -> 4, //El Calafate
"ASW" -> 4, //Abu Simbel Egypt
"HBE" -> 4, //Alexandria
"MRE" -> 4, //Maasai Mara National Reserve Kenya
"MFA" -> 4, //Mafia Island TZ
"SZG" -> 4, //Salzburg
"BBK" -> 4,
"BOB" -> 4, //Bora Bora French Polynesia
"JNX" -> 4, //GR
"MMY" -> 4, //Miyako JP
"SMI" -> 4, //GR
"TMR" -> 4, //Ahaggar National Park
"WDH" -> 4, 
"YAS" -> 4, //Fiji
"YXY" -> 4, //Whitehorse
"EIS" -> 4, //BVI
"ZAG" -> 4, 
"TPP" -> 4, //PE
"RJM" -> 4, 
"AOK" -> 4, 
"TGZ" -> 3, //Tuxtla Gutiérrez
"FAI" -> 3, //Fairbanks
"LBU" -> 3, 
"GAY" -> 3, //Bodh Gaya
"CYB" -> 3, //West End
"FCA" -> 3, //Glacier National Park
"MHH" -> 3, //Marsh Harbour Bahammas
"GGT" -> 3, //Bahamas
"GHB" -> 3, //Governor's Harbour Bahamas
"ORN" -> 3, //Oran 
"TNJ" -> 3, //Bintan Island, ID
"RAR" -> 3, //Cook Islands
"PJM" -> 3, //Costa rica
"LEU" -> 3, //Andora
"GSM" -> 3, //Iran
"TIR" -> 2, //Tirumala Venkateswara Temple
"FAT" -> 2, //Yosemite National Park USA
"DED" -> 2, //Rishikesh and Uttarakhand
"STM" -> 2, //Amazon
"AEY" -> 2, //Thingvellir National Park Iceland
"DCF" -> 2, //Dominica
"GOH" -> 2, 
"LIO" -> 2, 
"PRI" -> 2, //Seychelles
"PTF" -> 2, 
"LED" -> 2, //Andorra
"KVG" -> 2, //PG
"SPR" -> 2, //Belize
"RIH" -> 2, //PA
"RTB" -> 2, //Roatan
"KGC" -> 2, //Kangroo Island
"AXA" -> 2, 
"BLJ" -> 1, //Timgad & Batna
"GBJ" -> 1, //Guadaloupe
"MNF" -> 1, //Fiji
"MQS" -> 1, 
"PTF" -> 1, //Fiji
"SAB" -> 1, 
"YFB" -> 1, //Iqaluit
"SCT" -> 1, //Socotra Islands
"HLE" -> 1, //St Helena
"HZK" -> 1, //IS
    ),
    VACATION_HUB -> Map[String, Int](
"CJU" -> 240, //Jeju City
"CTS" -> 160, //Chitose / Tomakomai
"SYD" -> 128, //Sydney Australia
"MEL" -> 118, //Melbourne
"MCO" -> 104, //Orlando
"OKA" -> 90, //Naha
"PMI" -> 82, //Palma De Mallorca
"JED" -> 80, //Jeddah
"TRD" -> 78, //Trondheim
"HNL" -> 68, //Honolulu
"LAS" -> 68, //Las Vegas
"OGG" -> 68, //Kahului
"CTG" -> 68, //Cartagena
"CUN" -> 65, //Cancún
"POA" -> 60, //Porto Alegre
"AER" -> 60, //Sochi
"FUK" -> 59, //Fukuoka
"OOL" -> 57, //Gold Coast
"AGP" -> 55, //Málaga
"YVR" -> 55, //Vancouver
"CGH" -> 55, //São Paulo
"ITM" -> 54, //Osaka Japan
"CTA" -> 52, //Catania
"BAH" -> 51, //Manama
"PMC" -> 51, //Puerto Montt
"LPA" -> 50, //Gran Canaria Island
"BGO" -> 50, //Bergen
"PKX" -> 50, //Beijing China
"GRU" -> 49, //São Paulo
"BNE" -> 49, //Brisbane
"KOS" -> 48, //Sihanukville
"PMV" -> 48, //Isla Margarita
"MLA" -> 48, //Valletta
"MSY" -> 47, //New Orleans
"SAW" -> 46, //Istanbul
"REC" -> 46, //Recife
"PUJ" -> 45, //Punta Cana
"DMK" -> 45, //Bangkok
"TFS" -> 45, //Tenerife Island
"TFN" -> 45, //Tenerife Island
"HAN" -> 45, //Hanoi
"FAO" -> 45, //Faro
"OLB" -> 45, //Olbia (SS)
"HBA" -> 45, //Hobart
"BKI" -> 44, //Kota Kinabalu
"BNA" -> 43, //Nashville
"KUL" -> 42, //Kuala Lumpur
"AEP" -> 42, //Buenos Aires
"CAG" -> 42, //Cagliari
"TOS" -> 42, //Tromsø
"MHD" -> 42, //Mashhad
"YIA" -> 42, //Yogyakarta
"KRR" -> 42, //Krasnodar
"THR" -> 42, //Tehran
"FLN" -> 41, //Florianópolis
"GRO" -> 41, //Girona
"SGN" -> 40,
"CNS" -> 40, //Cairns
"SJD" -> 40, //San José del Cabo
"SHJ" -> 40, //Sharjah AE
"HAK" -> 40, //Haikou
"SVG" -> 40, //Stavanger
"BTH" -> 39, //Batam Island
"BOG" -> 39, //Bogota
"MAD" -> 38, //Madrid
"SVO" -> 38, //Moscow
"PVR" -> 37, //Puerto Vallarta
"LYS" -> 37, //Lyon
"CPT" -> 36, //Cape Town
"AKL" -> 36, //Auckland
"PMO" -> 36, //Palermo
"ORY" -> 36, //Paris
"OKD" -> 36, //Sapporo
"FLL" -> 36, //Miami
"TSV" -> 36, //Townsville
"BKK" -> 35, //Bangkok
"PER" -> 35, //Perth
"MED" -> 35, //Medina
"PTP" -> 35, //Pointe-Ã -Pitre
"DBV" -> 35, //Dubrovnik
"BAR" -> 35, //Qionghai
"ALC" -> 35, //Alicante
"DCA" -> 35, //Washington
"VAR" -> 34, //Varna
"RSW" -> 34, //Fort Myers
"GDN" -> 34, //GdaÅ„sk
"CHC" -> 34, //Christchurch
"LVI" -> 33, //Livingstone
"KIH" -> 33, //Kish Island IR
"GIG" -> 32, //Rio De Janeiro
"LIS" -> 32, //Lisbon
"LCA" -> 32, //Larnarca
"FNC" -> 32, //Funchal
"YUL" -> 32, //Montreal
"IGU" -> 32, //Foz Do IguaÃ§u
"LIH" -> 32, //Lihue
"RUH" -> 32,
"SYX" -> 31, //Sanya
"SHA" -> 31, //Shanghai China
"CNF" -> 31, //Belo Horizonte
"CWB" -> 31, //Curitiba
"BCN" -> 30, //Barcelona
"BER" -> 30, //Berlin
"MPH" -> 30, //Malay
"ADB" -> 30, //Izmir
"ARN" -> 30, //Stockholm
"ZQN" -> 30, //Queenstown
"SJU" -> 30, //San Juan
"PSA" -> 30, //Pisa
"CFU" -> 30, //Kerkyra Island
"RUN" -> 30, //St Denis
"VKO" -> 30, //Moscow
"MRS" -> 30, //Marseille
"FDF" -> 30, //Fort-de-France
"KZN" -> 30, //Kazan
"LOP" -> 30, //Mataram
"ADZ" -> 30, //San Andrés
"VLC" -> 30, //Valencia
"RAK" -> 29, //Marrakech
"MBJ" -> 29, //Montego Bay
"IGR" -> 29, //Puerto Iguazu
"DEL" -> 29,
"VIX" -> 29, //Vitória
"LED" -> 28, //St. Petersburg
"GYD" -> 28, //Baku
"KOA" -> 28, //Kailua-Kona
"CEB" -> 27, //Lapu-Lapu City
"FLR" -> 27, //Firenze
"LGA" -> 27, //New York
"REU" -> 27, //Reus
"MID" -> 27, //Mérida
"SKD" -> 26, //Samarkand
"BRC" -> 26, //San Carlos de Bariloche
"OSL" -> 25, //Oslo
"IBZ" -> 25, //Ibiza
"STT" -> 25, //Charlotte Amalie
"VFA" -> 25, //Victoria Falls
"SXR" -> 25, //Srinagar
"KWL" -> 25, //Guilin City
"HIJ" -> 25, //Hiroshima
"LIN" -> 25, //Milan Italian Alps
"CIA" -> 24, //Ostia Antica Italy
"ACE" -> 24, //Lanzarote Island
"NAS" -> 24, //Nassau
"BPS" -> 24, //Porto Seguro
"AJA" -> 24, //Ajaccio/NapolÃ©on Bonaparte
"NCE" -> 23, //Nice
"MAH" -> 23, //Menorca Island
"SDQ" -> 23, //Santo Domingo
"SXM" -> 22, //Saint Martin
"MFM" -> 22, //Macau
"CJC" -> 22, //Calama
"SMR" -> 22, //Santa Marta
"FOR" -> 22, //Fortaleza
"PXO" -> 22, //Peneda-Gerês National Park Portugal
"HAM" -> 22, //Hamburg
"NVT" -> 22, //Navegantes
"IKT" -> 22, //Irkutsk
"CXB" -> 22,
"GMP" -> 22, //Seoul
"SSA" -> 21, //Salvador
"JMK" -> 21, //Mykonos Island
"TPA" -> 21, //Tampa
"SIP" -> 21, //Simferopol
"BUF" -> 21, //Buffalo
"CTM" -> 21, //Chetumal
"HTI" -> 21, //Hamilton Island Resort
"ENO" -> 21, //Encarnación
"PLS" -> 21, //Providenciales Turks and Caicos
"RVN" -> 21, //Rovaniemi
"TFU" -> 21, //Chengdu
"DAD" -> 20, //Da Nang
"CXR" -> 20, //Nha Trang
"HRG" -> 20, //Hurghada
"SSH" -> 20, //Sharm el-Sheikh
"SCL" -> 20, //Santiago
"SAI" -> 20, //Siem Reap
"FUE" -> 20, //Fuerteventura Island
"AUA" -> 20, //Oranjestad
"CUR" -> 20, //Willemstad
"KRK" -> 20, //Kraków
"COV" -> 20, //Mersin TR
"LIR" -> 20, //Liberia Costa Rica
"SPU" -> 20, //Split
"UVF" -> 20, //Vieux Fort
"BRI" -> 20, //Bari
"BOS" -> 20,
"SDU" -> 20, //Rio De Janeiro
"KMQ" -> 20, //Kumamoto
"STI" -> 20, //Santiago
"CCK" -> 20,
"KNH" -> 20, //Kinmen
"ECN" -> 20, //Nicosia
"GOX" -> 19, //Goa IN
"AMM" -> 19, //Amman
"ASP" -> 19, //Alice Springs
"NQN" -> 19, //Neuquen
"RNO" -> 19, //Reno
"BWI" -> 19, //Washington
"BIA" -> 19, //Bastia-Poretta
"ITO" -> 19, //Hilo
"PBI" -> 19,
"NBO" -> 18, //Nairobi
"ANU" -> 18, //St. John's
"INN" -> 18, //Innsbruck
"YYC" -> 18, //Calgary
"OTP" -> 18, //Bucharest
"KTT" -> 18, //Kittilä FI
"KIN" -> 17, //Kingston
"NLU" -> 17, //Mexico City
"RVN" -> 17, //Rovaniemi FI
"CGB" -> 17, //Cuiabá
"QSR" -> 16, //Amalfi coast
"HUX" -> 16, //Huatulco
"JAI" -> 16, //Jaipur
"PHL" -> 16,
"GCM" -> 16, //Georgetown
"LLA" -> 16, //LuleÃ¥
"PPP" -> 16, //Whitsunday Coast Airport
"RKT" -> 16,
"ADL" -> 16, //Adelaide, AU
"ECN" -> 16, //TR Cyprus
"BJV" -> 15, //Bodrum
"PEN" -> 15, //Penang
"TIV" -> 15, //Tivat
"JNB" -> 15, //Johannesburg
"BIO" -> 15, //Bilbao
"TGZ" -> 15, //Tuxtla Gutiérrez
"IOS" -> 15, //Ilhéus
"BLQ" -> 15, //Bologna
"ISG" -> 15, //Ishigaki JP
"SNA" -> 15, //Santa Ana
"XMN" -> 15, //Xiamen
"ZIA" -> 15, //Moscow
"BSB" -> 15, //Brasília
"NGO" -> 15, //Tokoname
"YOW" -> 15, //Ottawa
"KGS" -> 14, //Kos Island
"GOI" -> 14, //Vasco da Gama
"BOJ" -> 14, //Burgas
"BON" -> 14, //Kralendijk Bonaire
"MCZ" -> 14,
"XCH" -> 14,
"MDQ" -> 14,
"BME" -> 14, //Broome
"VBY" -> 14, //Visby, SE
"YHZ" -> 14, //Halifax
"EYW" -> 13, //Key West
"EFL" -> 13, //Kefallinia Island
"YYT" -> 13, //St John
"CHQ" -> 12, //Heraklion
"LPQ" -> 12, //Luang Phabang
"BVC" -> 12, //Rabil
"COK" -> 12, //Kochi
"ZTH" -> 12, //Zakynthos Island
"XIY" -> 12, //Terracotta Army China
"VNS" -> 12, //Varanasi
"BAQ" -> 12, //Barranquilla
"BDS" -> 12, //Brindisi
"VCP" -> 12, //Campinas
"CAT" -> 12, //Lisbon
"MZG" -> 12, //TW
"SRQ" -> 12, //Sarasota/Bradenton
"PNQ" -> 12, //Pune
"TBZ" -> 12, //Iran
"CTU" -> 11, //Chengdu
"LAP" -> 10, //La Paz
"PDL" -> 10, //Azores
"JTR" -> 10, //Santorini Island
"SID" -> 10, //Espargos
"PLZ" -> 10, //Addo Elephant National Park South Africa
"MUB" -> 10, //Maun
"STX" -> 10, //Christiansted
"MQP" -> 10, //Mpumalanga
"TER" -> 10, //Azores Lajes
"AYQ" -> 10, //Ayers Rock
"IXB" -> 10, //Bagdogra Darjeeling
"AGX" -> 10, //Agatti
"BJL" -> 10, //Banjul
"FSC" -> 10, //Figari Sud-Corse
"GRQ" -> 10, //Grenoble French Alps
"ISG" -> 10, //Ishigaki
"KNO" -> 10, //North Sumatra
"CLY" -> 10, //Calvi-Sainte-Catherine
"NAP" -> 9, //Nápoli
"SPC" -> 9,
"FTE" -> 9, //El Calafate
"GYN" -> 9, //Goiânia
"IXZ" -> 9, //Port Blair
"KTA" -> 9, //Blue Mountains National Park Australia
"SAN" -> 9, //San Diego USA
"TRN" -> 9, //Turin Italian Alps
"YYJ" -> 9,
"ZAD" -> 9, //Zemunik (Zadar)
"SZG" -> 8, //Salzburg Austrian Alps
"SZG" -> 8, //Berchtesgaden National Park Germany
"TIR" -> 8, //Tirumala Venkateswara Temple
"MAA" -> 8, //Chennai
"KTN" -> 8, //Ketchikan
"SBZ" -> 8, //Sibiu
"VOG" -> 8, //Volgograd
"MYR" -> 8, //Myrtle Beach
"YLW" -> 8, //Jasper National Park Canada
"BTV" -> 8, //Burlington Stowe/Sugarbush Vermont USA
"JER" -> 8, //Guernsey
"LMP" -> 8, //Italy
"SLZ" -> 8, //São Luís
"YXC" -> 8, //Banff National Park Canada
"TPS" -> 8, //IT
"BZR" -> 8, //FR
"GMZ" -> 7, //Canary Islands
"FOC" -> 7, //Fuzhou
"IXC" -> 7, //Chandigarh
"ATQ" -> 7, //Amritsar
"FEN" -> 7, //Fernando De Noronha
"BZN" -> 7, //Bozeman
"FSZ" -> 7, //Fuji-Hakone-Izu National Park Japan
"GCI" -> 7, //Jersey
"MUH" -> 7, //El Alamein EG
"YDF" -> 7, //Gros Morne National Park Canada
"GPT" -> 7, //Gulf port
"LKO" -> 6, //Lucknow
"NKG" -> 6, //Nanjing
"DYG" -> 6,
"TSN" -> 6, //Tianjin
"VER" -> 6, //Pico de Orizaba National Park Mexico
"MTJ" -> 6, //Montrose (Ski resort)
"THE" -> 6, //Teresina
"ECP" -> 6, //Panama City Beach
"PNL" -> 6, //Italy
"YKS" -> 6, //Serbia
"TSN" -> 6, //Tainan TW
"STS" -> 6,
"NTQ" -> 6, //Wajima JP
"NSN" -> 6,
"IXU" -> 6, //Ellora caves
"HDS" -> 5, //Kruger National Park South Africa
"FAI" -> 5, //Fairbanks
"FAT" -> 5, //Yosemite National Park USA
"DLC" -> 5, //Dalian
"SHE" -> 5, //Shenyang
"JAC" -> 5, //Jackson
"CLQ" -> 5, //Nevado de Colima National Park Mexico
"IPC" -> 5, //Isla De Pascua
"LDH" -> 5,
"NLK" -> 5,
"SUV" -> 5,
"YTY" -> 5, //Yangzhou
"PKU" -> 5, //Pekanbaru ID
"AMD" -> 4, //Ahmedabad
"IOM" -> 4, //Isle of Man
"UNA" -> 4, //Transamérica Resort Comandatuba Island
"CSX" -> 4, //Changsha
"HRB" -> 4, //Harbin
"ASE" -> 4, //Aspen
"VQS" -> 4, //Vieques PR
"ACV" -> 4, //Eureka
"CMF" -> 4, //Chambéry
"CUK" -> 4, //Belize
"DBB" -> 4, //EG
"HHH" -> 4, //Hilton Head Island
"KUM" -> 4,
"LSI" -> 4, //Shetland
"SLK" -> 4,
"DLU" -> 4, //Dali CN
"SDS" -> 4, //Sado JP
"LEI" -> 4, //ES
"PQQ" -> 4,
"DED" -> 3, //Rishikesh and Uttarakhand
"ZUH" -> 3, //Zhuhai
"EGE" -> 3, //Vail/Beaver Creek Colorado USA
"SUN" -> 3, //Hailey Sun Valley Idaho USA
"SGU" -> 3, //Zion National Park
"CHS" -> 3,
"CNY" -> 3, //Arches National Park USA
"HDN" -> 3, //Hayden Steamboat Springs Colorado USA
"HAC" -> 3,
"HGL" -> 3,
"MFR" -> 3,
"YYB" -> 3, //North Bay
"HYA" -> 2, //Cape Cod
"MFR" -> 2, //Crater lake
"OTH" -> 2, //North Bend
"TVC" -> 2, //Traverse City
"CPX" -> 2, //Culebra PR
"ACK" -> 2, //Nantucket
"BRW" -> 2,
"BHB" -> 2, //Acadia NP
"GRB" -> 2, //Door County WI
"YQA" -> 2, //Muskoka CA
"OIM" -> 2, //JP
 ),
    FINANCIAL_HUB -> Map[String, Int](
"SIN" -> 65, //Singapore
"LHR" -> 60, //London
"PVG" -> 58, //Shanghai
"ICN" -> 55, //Seoul
"JFK" -> 53, //New York
"FRA" -> 52, //Frankfurt
"HKG" -> 50, //Hong Kong
"DXB" -> 50, //Dubai
"NRT" -> 48, //Tokyo
"TPE" -> 47, //Taipei
"ORD" -> 45, //Chicago
"KUL" -> 45, //Kuala Lumpur
"HND" -> 44, //Tokyo
"BOM" -> 44, //Mumbai
"SFO" -> 44, //San Francisco
"MUC" -> 43, //Munich
"SZX" -> 42, //Shenzhen
"DFW" -> 42, //Dallas Fort Worth
"AUH" -> 41, //Abu Dhabi
"JNB" -> 41, //Johannesburg
"MAD" -> 41, //Madrid
"CAN" -> 40, //Guangzhou
"AMS" -> 40, //Amsterdam
"GVA" -> 40, //Geneva
"SYD" -> 39, //Sydney
"LGW" -> 39, //London
"GRU" -> 38, //Sao Paulo
"DEN" -> 38, //Denver
"ZRH" -> 38, //Zurich
"SCL" -> 37, //Santiago
"BOG" -> 37, //Bogota
"LAX" -> 36, //Los Angeles
"YVR" -> 35, //Vancouver
"CDG" -> 34, //Paris
"DOH" -> 34, //Doha
"MEL" -> 34, //Melbourne
"BER" -> 34, //Berlin
"KIX" -> 33, //Osaka
"KWI" -> 33, //Kuwait City
"TLV" -> 33, //Tel Aviv
"CGK" -> 33, //Jakarta
"PEK" -> 32, //Beijing
"EWR" -> 32, //New York
"CPH" -> 32, //Copenhagen
"ITM" -> 32, //Osaka
"YUL" -> 32, //Montreal
"DUB" -> 31, //Dublin
"DME" -> 31, //Moscow
"BOS" -> 30, //Boston
"BKK" -> 30, //Bangkok
"SGN" -> 30, //Ho Chi Minh City
"YYZ" -> 29, //Toronto
"AKL" -> 29, //Auckland
"SVO" -> 29, //Moscow
"FUK" -> 29, //Fukuoka
"CLT" -> 29, //Charlotte
"MIA" -> 29, //Miami
"VIE" -> 28, //Vienna
"SEA" -> 28, //Seattle
"PHX" -> 28, //Phoenix
"EZE" -> 27, //Buenos Aires
"SHA" -> 27, //Shanghai
"IAH" -> 27, //Houston
"LGA" -> 26, //New York
"ARN" -> 26, //Stockholm
"OSL" -> 26, //Oslo
"IST" -> 26, //Istanbul
"FCO" -> 26, //Rome
"MXP" -> 25, //Milan
"PUS" -> 25, //Busan
"ATL" -> 25, //Atlanta
"BAH" -> 25, //Bahrain
"CPT" -> 25, //Cape Town
"ORY" -> 25, //Paris
"YYC" -> 25, //Calgary
"PKX" -> 24, //Beijing
"LUX" -> 24, //Luxembourg
"GMP" -> 24, //Seoul
"MEX" -> 24, //Mexico City
"RUH" -> 24, //Riyadh
"DUS" -> 24, //Dusseldorf
"DCA" -> 23, //Washington DC
"GIG" -> 22, //Rio de Janeiro
"LOS" -> 21, //Lagos
"NGO" -> 21, //Nagoya
"LAS" -> 21, //
"BUD" -> 20, //Budapest
"HAJ" -> 20, //Hanover
"LCY" -> 20, //London
"LIN" -> 20, //Milan
"LIM" -> 20, //Lima
"BCN" -> 20, //Barcelona
"BRU" -> 19, //Brussels
"JED" -> 19, //Jeddah
"PRG" -> 19, //Prague
"WAW" -> 19, //Warsaw
"TAS" -> 19, //Tashkent
"DTW" -> 19, //Detroit
"DEL" -> 18, //New Delhi
"BLQ" -> 18, //Bologna
"BSB" -> 18, //Brasilia
"CGN" -> 18, //Cologne
"MSP" -> 18, //Minneapolis
"CMN" -> 18, //Casablanca
"SLC" -> 18, //Salt Lake City
"HAM" -> 17, //Hamburg
"IAD" -> 17, //Washington DC
"MNL" -> 17, //Manila
"HEL" -> 17, //Helsinki
"BLR" -> 16, //Bangalore
"ALG" -> 16, //Algiers
"TLL" -> 16, //Tallinn
"HAN" -> 16, //Hanoi
"DAL" -> 16, //Dallas
"PHL" -> 16, //Philadelphia
"CGH" -> 15, //Sao Paulo
"MDW" -> 15, //Chicago
"RMO" -> 15, //Chisinau
"RTM" -> 15, //The Hague
"PTY" -> 15, //Panama City
"HYD" -> 15, //Hyderabad
"SAN" -> 15, //San Diego
"EDI" -> 14, //Edinburgh
"BNE" -> 14, //Brisbane
"IKA" -> 14, //Tehran
"KHH" -> 14, //Kaohsiung
"PER" -> 14, //Perth
"SJC" -> 14, //San Francisco
"TRN" -> 14, //Turin
"DMK" -> 14, //Bangkok
"TFU" -> 14, //Chengdu
"ALA" -> 13, //Almaty
"TLS" -> 13, //Toulouse
"CBR" -> 12, //Canberra
"LEJ" -> 12, //Leipzig
"TAO" -> 12, //Qingdao
"VNO" -> 12, //Vilnius
"YQB" -> 12, //Quebec City
"ADL" -> 12, //Adelaide
"VKO" -> 12, //
"BWI" -> 11, //Baltimore
"FLL" -> 11, //
"KUN" -> 11, //Kaunas
"LED" -> 11, //St Petersburg
"KMG" -> 11, //
"TPA" -> 11, //Tampa
"STR" -> 10, //Stuttgart
"AEP" -> 10, //Buenos Aires
"BNA" -> 10, //Nashville
"MAA" -> 10, //Chennai
"RIX" -> 10, //Riga
"SDU" -> 10, //Rio de Janeiro
"TSA" -> 10, //Taipei
"YEG" -> 10, //Edmonton
"CTU" -> 10, //Chengdu
"AMD" -> 9, //GIFT City-Gujarat
"TSN" -> 9, //Tianjin
"ATH" -> 9, //Athens
"AUS" -> 9, //Austin
"BEG" -> 9, //Belgrade
"BGO" -> 9, //Bergen
"MDE" -> 9, //Medellin
"MLA" -> 9, //Malta
"NBO" -> 9, //Nairobi
"YOW" -> 9, //Ottawa
"PDX" -> 9, //Portland
"WLG" -> 9, //Wellington
"MAN" -> 8, //Manchester
"BDA" -> 8, //Bermuda
"BTS" -> 8, //Bratislava
"CKG" -> 8, //Jakarta
"DUR" -> 8, //Durban
"GOT" -> 8, //Gothenburg
"KHI" -> 8, //Karachi
"LYS" -> 8, //Grenoble
"NCL" -> 8, //Newcastle
"NKG" -> 8, //Nanjing
"NLU" -> 8, //Mexico City
"POS" -> 8, //Port of Spain
"ESB" -> 8, //Ankara
"ANC" -> 7, //Anchorage
"DLC" -> 7, //Dalian
"GYD" -> 7, //Baku
"NQZ" -> 7, //Nur-Sultan
"PNQ" -> 7, //Pune
"STL" -> 7, //
"MTY" -> 7, //Monterrey
"PEN" -> 7, //
"AAL" -> 6, //Aalborg
"NAS" -> 6, //Nassau
"OAK" -> 6, //San Francisco
"PIT" -> 6, //Pittsburgh
"SMF" -> 6, //Sacramento
"SOF" -> 6, //Sofia
"ADD" -> 5, //Addis Ababa
"AHB" -> 5, //
"CLO" -> 5, //Cali
"DTM" -> 5, //Dortmund
"GLA" -> 5, //Glasgow
"KGL" -> 5, //Kigali
"TRD" -> 5, //Trondheim
"YTZ" -> 5, //Toronto
"CZX" -> 5, //Changzhou
"MBA" -> 5, //Mombasa
"GDL" -> 5, //
"MVD" -> 5, //
"HGH" -> 4, //Hangzhou
"BGI" -> 4, //Bridgetown
"DMM" -> 4, //
"HOU" -> 4, //Houston
"KEF" -> 4, //Reykjavik
"SNA" -> 4, //
"YXE" -> 4, //Saskatoon
"HAV" -> 4, //Havanna
"XIY" -> 3, //Xi'an
"IOM" -> 3, //Castletown
"BHX" -> 3, //Birmingham
"ABV" -> 3, //
"JNU" -> 3, //Juneau
"MCI" -> 3, //
"YWG" -> 3, //Winnipeg
"YQR" -> 3, //Regina
"LAD" -> 3, //Luanda
"SJJ" -> 3, //Sarajevo
"LCA" -> 3, //Nicosia
"WUH" -> 3, //Wuhan
"RDU" -> 3, //
"SDJ" -> 3, //Sendai
"CCU" -> 3, //
"ABQ" -> 2, //
"BOI" -> 2, //
"BUR" -> 2, //
"CLE" -> 2, //
"EBL" -> 2, //Arbil
"JAX" -> 2, //
"MKE" -> 2, //
"YWG" -> 2, //
"PZU" -> 2, //Port Sudan
"JIB" -> 2, ////Djibouti
"CAY" -> 2, //
"AOJ" -> 2, //Aomori
"SKG" -> 2, //
"CMH" -> 2, //
"IND" -> 2, //
"TAE" -> 2, //
"CXH" -> 1, //Vancouver Heliport
"JRA" -> 1, //NYC Heliport
"JRB" -> 1, //NYC Heliport
"SKM" -> 1, //Sao Paulo, fictional IATA code
"HHP" -> 1, //Hong Kong
"SDB" -> 1, //Rio Heliport
"NJA" -> 1, //Tokyo Heliport
    ), 
    DOMESTIC_AIRPORT -> Map(
      "LGA" -> 0,
      "DCA" -> 0,
      "MDW" -> 0,
      "SNA" -> 0,
      "BUR" -> 0,
      "OAK" -> 0,
      "DAL" -> 0,
      "HOU" -> 0,
      "AZA" -> 0,
      "COS" -> 0,
      "PAE" -> 0,
      "PIE" -> 0,
      "SFB" -> 0,
      "USA" -> 0,
      "PGD" -> 0,
      "OGD" -> 0,
      "LIH" -> 0,
      "OGG" -> 0,
      "CAK" -> 0,
      "ORH" -> 0,
      "SIG" -> 0,
      //canada
      "YTZ" -> 0,
      "YHU" -> 0,
      //mexico
      "TLC" -> 0,
      "CJS" -> 0,
      //EU
      "EIN" -> 0,
      "CRL" -> 0,
      "ANR" -> 0,
      "BVA" -> 0,
      "HHN" -> 0,
      "LBC" -> 0,
      "FKB" -> 0,
      "NRN" -> 0,
      "BRE" -> 0,
      "DTM" -> 0,
      "FMM" -> 0,
      "REU" -> 0,
      "GRO" -> 0,
      "LIN" -> 0,
      "CIA" -> 0,
      "TSF" -> 0,
      "NYO" -> 0,
      "BMA" -> 0,
      "TRF" -> 0,
      "WMI" -> 0,
      "CAT" -> 0,
      "AGH" -> 0,
      "ORK" -> 0,
      //GB
      "BHD" -> 0,
      //iceland
      "RKV" -> 0,
      //china
      "TSN" -> 0,
      "WNZ" -> 0,
      "SHA" -> 0,
      "ZUH" -> 0,
      "LHW" -> 0,
      "LXA" -> 0,
      "HUZ" -> 0,
      "FUO" -> 0,
      "CTU" -> 0,
      //japan
      "ITM" -> 0,
      "UKB" -> 0,
      "IBR" -> 0,
      "OKD" -> 0,
      //korea
      "GMP" -> 0,
      "USN" -> 0,
      //argentina
      "AEP" -> 0,
      //brazil
      "CGH" -> 0,
      "SDU" -> 0,
      //colombia
      "EOH" -> 0,
      "FLA" -> 0,
      //chile
      "LSC" -> 0,
      //dominican-republic
      "JBQ" -> 0,
      //iran
      "THR" -> 0,
      "PGU" -> 0,
      "ABD" -> 0,
      "KIH" -> 0,
      "AWZ" -> 0,
      //india
      "HDO" -> 0,
      "DHM" -> 0,
      "BDQ" -> 0,
      "PNY" -> 0,
      "AIP" -> 0,
      "STV" -> 0,
      "KNU" -> 0,
      "NAG" -> 0,
      //russia
      "CEK" -> 0,
      "KEJ" -> 0,
      "BTK" -> 0,
      "YKS" -> 0,
      "UUS" -> 0,
      //southern africa
      "HLA" -> 0,
      "ERS" -> 0,
      //indonesia
      "HLP" -> 0,
      //Australia
      "AVV" -> 0,
      "MCY" -> 0,
      "LST" -> 0
    )
  ) + (GATEWAY_AIRPORT -> getGatewayAirports().map(iata => (iata, 0)).toMap) + (ELITE_CHARM -> getEliteDestinations())

  patchFeatures()

  def patchFeatures() = {
    val airportFeatures = scala.collection.mutable.Map[String, ListBuffer[AirportFeature]]()
    featureList.foreach {
      case (featureType, airportMap) =>
        airportMap.foreach {
          case (airportIata, featureStrength) =>
            val featuresForThisAirport = airportFeatures.getOrElseUpdate(airportIata, ListBuffer[AirportFeature]())
            featuresForThisAirport += AirportFeature(featureType, featureStrength)
        }
    }


    airportFeatures.toList.foreach {
        case (iata, features) =>
          AirportSource.loadAirportByIata(iata) match {
            case Some(airport) =>
              AirportSource.updateAirportFeatures(airport.id, features.toList)
            case None =>
              println(s"Cannot find airport with iata $iata to patch $features")
          }
      }
      IsolatedAirportPatcher.patchIsolatedAirports()
  }

    def getEliteDestinations() : Map[String, Int] = {
      val destinations = DestinationSource.loadAllDestinations()
      val iataMap = destinations.groupBy(_.airport.iata).view.mapValues(_.length).toMap
      println("inserting elite destinations to features...")
      println(iataMap)
      iataMap
    }


  def getGatewayAirports() : List[String] = {
    //The most powerful airport of every country
    val airportsByCountry = AirportSource.loadAllAirports().groupBy(_.countryCode).filter(_._2.length > 0)
    val topAirportByCountry = airportsByCountry.view.mapValues(_.sortBy(_.power).last)

    val baseList = topAirportByCountry.values.map(_.iata).toList

    val list: mutable.ListBuffer[String] = collection.mutable.ListBuffer(baseList:_*)

    list -= "CGO" //China
    list -= "OSS" //Uzbekistan
    list += "FRU"
    list -= "LHE" //Pakistan
    list -= "OKZ"
    list += "VTE" //Laos
    list += "ISB"
    list -= "GYE" //Ecuador
    list += "UIO"
    list -= "THR" //Iran
    list += "IKA"
    list -= "RUH" //Saudi
    list += "JED"
    list -= "OND" //Namibia
    list += "WDH"
    list -= "ZND" //Mali
    list += "NIM"
    list -= "BYK" //Ivory Coast
    list += "ABJ"
    list -= "DLA" //Cameroon
    list += "NSI"
    list -= "MQQ" //Chad
    list += "NDJ"
    list -= "BLZ" //Malawi
    list += "LLW"
    list -= "KGA" //DRC
    list -= "MJM"
    list += "FIH"
    list -= "KAN" //Nigeria
    list += "LOS"
    list -= "APL" //Mozambique
    list += "MPM"
    list -= "MWZ" //Tanzania
    list += "DAR"
    list -= "HGU" //PNG
    list += "POM"
    list -= "STX" //US VI
    list += "STT"
    list -= "XSC" //
    list += "PLS"
    list += "NZF" //Pegasus Airfield, AQ (fictional IATA)
    list += "PPT"
    list += "NOU"
    list += "GOH" //Greenland


    //add extra ones for bigger countries
    list.appendAll(List(
      "NZF", //McMurdo AQ
      "CAN", //China
      "PVG",
      "PEK",
      "JFK", //US
      "LAX",
      "SFO",
      "MIA",
      "BOM", //India
      "RUH", //Saudi
      "AUH", //UAE
      "CPT", //South Africa
      "GIG", //Brazil
      "GRU",
      "NRT", //Japan
      "HND",
      "KIX",
      "SVO", //Russia
      "LED",
      "FCO", //Italy
      "MXP",
      "GOH", //Greenland / DK
      "MAD", //Spain
      "BCN",
      "FRA", //Germany
      "MUC",
      "SYD", //Australia
      "MEL",
      "YVR", //Canada
      "YUL",
      "YYZ"))
    list.toList
  }
}
