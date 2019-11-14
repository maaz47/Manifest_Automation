#setwd("~\\Manifest_Automation\\")
manisfest_path <- "Manifests_Live"

sys_admin_email <- "muhammad.khan@gerrysdnata.com"        #use your email in development phase
manifest_sender_email <- "muhammad.khan@gerrysdnata.com"  #use your email in development phase

db_server <- "localhost\\SQLEXPRESS"
db_name <- "cargo"
db_user <- "maaz"
db_pwd <- "sql@2018"
tablename <- "Manifests"                                  #For Development phase Use "Manifests"

con_Logs_path <- "DB_LOGS.csv"
DB_Logs_path <- "Manifest_Compiling_Uploading.log"

sheetname <- NULL
Pattern <- "FINAL_MANIFEST_" #add a regex 

#test