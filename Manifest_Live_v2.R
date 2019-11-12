# Assumptions
#   All excel files should be in same given format
#   All excel files should be named properly
#   All excel files should have manifest in 1st sheet and only this 1st sheet will be loaded
#   All special chars (except '/') in PIECES column are considered as Typo and are removed.
#   First column of all sheets should be AWB with (999-99999999)
#   File Name should have a standard.

# Format:
#   AWB | No.PCS	| DESCRIPTION	| SCC	| WT/KG	| ORG	| DEST	| RMRKS | STATUS



######## LIB

library(data.table)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)
library(odbc)
library(RDCOMClient)

################### VARIABLES ###################################################################################

#setwd("~\\Manifest_Automation\\")
manisfest_path <- "Manifests_Live"

sys_admin_email <- "muhammad.khan@gerrysdnata.com" #use your email when developing
manifest_sender_email <- "muhammad.khan@gerrysdnata.com" #use your email when developing
tablename <- "Manifests_Live"
con_Logs_path <- "DB_LOGS.csv"
DB_Logs_path <- "Manifest_Compiling_Uploading.log"

sheetname <- NULL
Pattern <- "FINAL_MANIFEST_" #add a regex 

#################################################################################################################

source("D:\\Internal_System\\Manifest_Automation\\Scripts\\cleaning_and_str_commodities.R")
source("D:\\Internal_System\\Manifest_Automation\\Scripts\\Reading_and Validating_files_in_bulk.R")


######## FILES READING, COMPILING, FILTERING VALID AWB


list_df <- read_all_files_in_dir(manisfest_path,Pattern,sheetname,manifest_sender_email)
a <- sapply(list_df, is.data.frame)
list_df <- list_df[which(a)]
list_df <- rbindlist(list_df)
list_df <- Valid_AWB(list_df, "X1")


list_df$Airline <- substr(trimws(list_df$Flight_No), 1, 2)

######## NAMING
names(list_df) <- c('AWB', 'PIECES', 'COMMODITY_DESC', 'SHC', 'WEIGHT', 'ORG', 'DST', 'RMK', 'STATUS_UNCLEANED', 'FILE_NAME', 'FLIGHT_NO', 'FLIGHT_DATE', 'AIRLINE')

list_df <- cleaning_and_str_commodities(list_df)


###################################################################
######### SQL SERVER ##############################################
###################################################################
dbWriteTable_output <- FALSE
con <-  tryCatch(
  dbConnect(
    odbc(),Driver = "SQL Server",Server = "localhost\\SQLEXPRESS",Database = "cargo",UID = "maaz",PWD = "sql@2018",Port = 1433
  ),
  error = function(e) {
    
    send_email(
      sys_admin_email,
      "ERROR IN MANIFEST AUTOMATION - SQL SERVER CONNECTION FAILED",
      paste("PLEASE CHECK DATA ENTRY FAIL IN SQL SERVER:",e,sep = '\n'),
      
    )
  }
)


#truncate <- odbc::dbSendQuery(con,paste("TRUNCATE TABLE Manifests;"))
dbWriteTable_output <- dbWriteTable(conn = con, tablename, list_df, append = TRUE)
if (!dbWriteTable_output) {
  send_email(
    sys_admin_email,
    "ERROR IN MANIFEST AUTOMATION - SQL SERVER INSERTION",
    "PLEASE CHECK DATA ENTRY FAIL IN SQL SERVER",
  )
}
dbDisconnect(con)
