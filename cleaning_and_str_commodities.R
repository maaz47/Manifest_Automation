str_commodities <- c(
  "PERISHABLE_EDIBLE_PEP_MANGOES"
  ,"PERISHABLE_EDIBLE_PEP_FRUITSANDVEG"
  ,"PERISHABLE_EDIBLE_PEM_MEAT"
  ,"PERISHABLE_EDIBLE_PES_SEAFOOD"
  ,"PERISHABLE_EDIBLE_EAT"
  ,"PERISHABLE_EDIBLE"
  ,"PERISHABLE_NONEDIBLE_PER"
  ,"PERISHABLE_NONEDIBLE_NMP"
  ,"PERISHABLE_NONEDIBLE_NWP"
  ,"NONEDIBLE"
  ,"PERISHABLE"
  ,"MACHINERY & PARTS"
  ,"MEDICAL"
  ,"DGR_EXPLOSIVES"
  ,"DGR_GASES"
  ,"DGR_ LIQUIDS"
  ,"DGR_FLAMMABLE SOLIDS"
  ,"DGR_OXIDIZING AGENTS"
  ,"DGR_TOXIC INFECTIOUS"
  ,"DGR_RADIOACTIVE"
  ,"DGR_CORROSIVES"
  ,"DGR_OTHERS"
  ,"LEATHER_GARMENTS"
  ,"LEATHER_FINISHED"
  ,"LEATHER"
  ,"TEXTILE_GARMENTS"
  ,"TEXTILE_HOME"
  ,"TEXTILE"
  ,"COURIER_MAIL"
  ,"COURIER"
  ,"AVI"
  ,"LHO_ORGANS"
  ,"HEG_HATCHINGEGGS"
  ,"HUM_HUMANREMAINS"
  ,"HUM_HUMANREMAINS"
  ,"SPORTSGOODS"
  ,"GEN/OTHER/DRY"
  ,"CONSOL"
)


cleaning_and_str_commodities <- function(list_df){
  ########CLEANING
  
  ####### NON ASCII REMOVING ##########################
  list_df$AWB <- iconv(list_df$AWB, "latin1", "ASCII", sub = "")
  list_df$PIECES <- iconv(list_df$PIECES, "latin1", "ASCII", sub = "")
  list_df$COMMODITY_DESC <- iconv(list_df$COMMODITY_DESC, "latin1", "ASCII", sub = "")
  list_df$SHC <- iconv(list_df$SHC, "latin1", "ASCII", sub = "")
  list_df$WEIGHT <- iconv(list_df$WEIGHT, "latin1", "ASCII", sub = "")
  list_df$DST <- iconv(list_df$DST, "latin1", "ASCII", sub = "")
  list_df$ORG <- iconv(list_df$ORG, "latin1", "ASCII", sub = "")
  list_df$RMK <- iconv(list_df$RMK, "latin1", "ASCII", sub = "")
  list_df$FILE_NAME <-
    iconv(list_df$FILE_NAME, "latin1", "ASCII", sub = "")
  list_df$Batch_date <- Sys.time()
  
  
  ####### PIECES ##########################
  list_df$Shipment_Pieces <- sub("/.*", "", list_df$PIECES)
  list_df$Total_Pieces <- sub(".*/", "", list_df$PIECES)
  
  list_df$Shipment_Pieces <- trimws(gsub("[!@#$%^&*()_=+/,.?\":;\'{}|<>-]" ,"" ,list_df$Shipment_Pieces))
  list_df$Total_Pieces <- trimws(gsub("[!@#$%^&*()_=+/,.?\":;\'{}|<>-]" , "" , list_df$Total_Pieces))
  
  ####### COMMODOTIY ##########################
  '%!in%' <- function(x, y)
    ! ('%in%'(x, y))
  
  list_df$COMMODITY_DESC <- trimws(gsub("[!@#$%^&*()_=+/,.?\":;\'{}|<>-]" , "" , list_df$COMMODITY_DESC))
  list_df$SHC <- trimws(gsub("[!@#$%^&*()_=+/.?\":;\'{}|<>-]" , "" , list_df$SHC))
  list_df$COMMODITY <- 'Unassigned'
  
  #########PERISHABLE
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("MANGO|MNGO", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "PERISHABLE_EDIBLE_PEP_MANGOES"
  #list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & !grepl("MANGO|MNGO", list_df$COMMODITY_DESC, ignore.case = TRUE) & grepl("PEP", list_df$SHC, ignore.case = TRUE)] <- "PERISHABLE_EDIBLE_PEP_MANGOES"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("VEG|VGET|FRU|FRIUT",list_df$COMMODITY_DESC,ignore.case = TRUE) | grepl("^PEP", list_df$SHC, ignore.case = TRUE)] <- "PERISHABLE_EDIBLE_PEP_FRUITSANDVEG"
  
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("CHILLED MEAT|MEET|EMAT|BEEF|COW|MUTTON|CHIC|GOAT", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("^PEM", list_df$SHC, ignore.case = TRUE)] <- "PERISHABLE_EDIBLE_PEM_MEAT"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("FISH|FSH|RAZOR|LOBS|PRAWN|CRAB|FROZEN|SEAFO|SEA F|SEA", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("^PES", list_df$SHC, ignore.case = TRUE)] <- "PERISHABLE_EDIBLE_PES_SEAFOOD"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^SWEET|SWET$|CANDY|FOOD", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("^EAT", list_df$SHC, ignore.case = TRUE)] <- "PERISHABLE_EDIBLE_EAT"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("EDIB|EDBL", list_df$COMMODITY, ignore.case = TRUE)] <- "PERISHABLE_EDIBLE"
  
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^PER", list_df$SHC, ignore.case = TRUE)] <- "PERISHABLE_NONEDIBLE_PER"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("NMP|MAGZ|MAGAz|MGZI|DIGES", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("^NMP$", list_df$SHC, ignore.case = TRUE)] <- "PERISHABLE_NONEDIBLE_NMP"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("NWP|NEWS|PAPER|PAPR", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("^NWP$", list_df$SHC, ignore.case = TRUE)] <- "PERISHABLE_NONEDIBLE_NWP"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("non.*edib|edib.*non", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "NONEDIBLE"
  
  
  
  #########HEAVY EQUIPMENT
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("EQUIP|EQUPM|MACHINE|SHIP|AIRCRAFT|AIR CRAFT|PARTS|TOOLS|ELECTRONIC|SPARE|ASSEMBLY|ASSMBLY|AUTO",list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "MACHINERYANDHEAVYEQUIPMENTS"
  
  ##################MEDICAL
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("MED|PHAR|DIAG|SURGIC|SUPPLEMENT|HERB|HEALTH|CARE|DENTAL",list_df$COMMODITY_DESC,ignore.case = TRUE) | grepl("^PIL$", list_df$SHC, ignore.case = TRUE)] <- "MEDICAL"
  
  
  #DGR
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^RCX$|^REX$|^RGX$|^RXB$|^RXC$|^RXD$|^RXE$|^RXG$|^RXS$", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("RCX|REX|RGX|RXB|RXC|RXD|RXE|RXG|RXS",list_df$SHC, ignore.case = TRUE)] <- "DGR_EXPLOSIVES"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^RFG$|^RNG$|^RPG$", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("RFG|RNG|RPG", list_df$SHC, ignore.case = TRUE)] <-"DGR_GASES"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^RCL$|^RFL$", list_df$COMMODITY_DESC, ignore.case = TRUE) |grepl("RCL|RFL", list_df$SHC, ignore.case = TRUE)] <- "DGR_ LIQUIDS"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^RFS$", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("RFS", list_df$SHC, ignore.case = TRUE)] <- "DGR_FLAMMABLE SOLIDS"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^ROX$", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("ROX", list_df$SHC, ignore.case = TRUE)] <- "DGR_OXIDIZING AGENTS"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^RPB$", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("RPB", list_df$SHC, ignore.case = TRUE)] <- "DGR_TOXIC INFECTIOUS"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^RRE$|^RRW$|^RRY$", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("RRE|RRW|RRY", list_df$SHC, ignore.case = TRUE)] <- "DGR_RADIOACTIVE"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^RCM$", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("RCM", list_df$SHC, ignore.case = TRUE)] <- "DGR_CORROSIVES"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^DGR$|^ELI$|^ELM$|^MAG$|^RDS$|^REQ$|^RFW$|^RIS$|^RLI$|^RLM$|^RMD$|^ROP$|^RSB$|^RSC$|OIL|CHEM|WATER|PETROL|SLICON|^PAINT$",list_df$COMMODITY_DESC,ignore.case = TRUE) | grepl( "DGR|ELI|ELM|MAG|RDS|REQ|RFW|RIS|RLI|RLM|RMD|ROP|RSB|RSC", list_df$SHC, ignore.case = TRUE)] <- "DGR_OTHERS"
  
  
  ############LEATHER
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("lea.*gar|gar.*lea", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "LEATHER_GARMENTS"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("lea.*sho|sho.*lea", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "LEATHER_GARMENTS"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("lea.*jack|jack.*lea", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "LEATHER_GARMENTS"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("lea.*fin|fin.*lea", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "LEATHER_FINISHED"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("LEA|LETH", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "LEATHER"
  
  ##############TEXTILE
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("GARMENT|GARMNT|GRMN|BAG|GLOV|FLEEC|PANT|LACE|SHOE|BOOT", list_df$COMMODITY_DESC, ignore.case = TRUE )] <- "TEXTILE_GARMENTS"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("tex.*hom|hom.*tex", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "TEXTILE_HOME"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl( "TOWEL|DENIM|DENM|Bed|Sheet|Curtains", list_df$COMMODITY_DESC,ignore.case = TRUE )] <- "TEXTILE_HOME"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("TXT|TEXT|FABRIC", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "TEXTILE"
  
  #############COURIER
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("MAIL", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "COURIER_MAIL"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("COU|CORI", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "COURIER"
  
  #HUMAN REMAINS & ORG
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("DOG|BIRD|HORSE|PET|CAT", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("^AVI$", list_df$SHC, ignore.case = TRUE)] <- "HRO_AVI"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("BLOOD|ORGAN|LHO|HUMAN", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("^LHO$", list_df$SHC, ignore.case = TRUE)] <- "HRO_LHO&ORGANS"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("EGG|HATCHIN|HEG", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("^HEG$", list_df$SHC, ignore.case = TRUE)] <- "HRO_HEG_HATCHINGEGGS"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("body.*dead|dead.*body", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("^HUM$", list_df$SHC, ignore.case = TRUE)] <- "HRO_HUM_HUMANREMAINS"
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("remain.*human|human.*remain", list_df$COMMODITY_DESC, ignore.case = TRUE) | grepl("^HUM$", list_df$SHC, ignore.case = TRUE)] <-"HRO_HUM_HUMANREMAINS"
  
  
  ##############SPORTS
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl( "SPORT|BALL|CRICK|CRCK|BAT|BADMINTON|MINTON|MNTON|RACKET|HOCKEY|SKATES", list_df$COMMODITY_DESC, ignore.case = TRUE )] <- "SPORTSGOODS"
  
  #OTHER/DRY
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl( "MUSWAK|MISWAK|TENT|TNT|SHEEP|PERSN|PERSON|PRSN|PARSN|EFF|ZIP|BOOK|CARPET|CRPT|CARP|COSMETIC|CRAFT|GEN", list_df$COMMODITY_DESC, ignore.case = TRUE )] <- "GEN/DRY"
  
  ##############EXHIBITIONGOODS
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("EXHIBIT|PAINT|PRINT|LABEL|TAG", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "EXHIBITIONGOODS"
  
  ##############XPS CARGO
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^XPS$", list_df$SHC, ignore.case = TRUE)] <- "XPSCARGO"
  
  ##############PERISHABLE
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("^PERISH", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "PERISHABLE"
  
  ##############CONSOL
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned' & grepl("Cnsl|CONS|Solidation", list_df$COMMODITY_DESC, ignore.case = TRUE)] <- "TEXTILE_GARMENTS"
  
  
  list_df$COMMODITY[list_df$COMMODITY == 'Unassigned'] <- "OTHER"
  
  
  ####### STATUS ##########################
  list_df$STATUS_UNCLEANED[which(is.na(list_df$STATUS_UNCLEANED))] <- "UPLIFTED"
  list_df$STATUS <- if_else(list_df$STATUS_UNCLEANED == "OFF", "OFFLOADED", "UPLIFTED")
  #list_df$STATUS <- if_else(is.na(list_df$STATUS_UNCLEANED), "UPLIFTED",  "OFFLOADED")
  
  
  ####### SUBSET ##########################
  list_df <- subset(list_df, select = -c(STATUS_UNCLEANED))
  
  return(list_df)
  
}
