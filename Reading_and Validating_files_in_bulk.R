library(data.table)
library(readxl)
library(openxlsx)
library(lubridate)
library(dplyr)
library(odbc)
library(htmlTable)

resend_files <- vector(mode = "character")
######## FUNCTIONS

send_email <- function(to, subject, contenct, att_path) {
  OutApp <- COMCreate("Outlook.Application")
  outMail = OutApp$CreateItem(0)
  outMail[["To"]] = to
  outMail[["subject"]] = subject
  outMail[["body"]] = contenct
  #outMail[["Attachments"]]$Add(att_path)
  outMail$Send()
}

read_file_x1 <- function(Path,sheetname) {

        sht <- readxl::read_excel(Path,range = 'A1:I300',col_names = c("X1","X2","X3","X4","X5","X6","X7","X8","X9"),trim_ws = TRUE,sheet = sheetname,col_types = "text"  )
        #dat <- trimws(sht$X6[sht$X1 == "FLIGHT NUMBER" & !is.na(sht$X1)][1])
        dat <- trimws(sht$X6[4])
        dat <- if_else(!is.na(as.numeric(dat)),as.Date(as.numeric(dat), origin = "1899-12-30"),dmy(dat))
        #no <- trimws(sht$X3[sht$X1 == "FLIGHT NUMBER" & !is.na(sht$X1)][1])
        no <- trimws(sht$X3[4])
        if(ncol(sht) == 9 
           && !is.na(dat)
           && !is.na(no)){
          sht[, c(1:ncol(sht))] <- lapply(sht[, c(1:ncol(sht))], trimws)
          sht$file_name <- paste(Path)
          sht$Flight_No <- no
          sht$Flight_date <- dat
          return(sht)
        }else {
          resend_files <<- c(resend_files,Path)
        }
    
}

#read_file <- function(Path,sheetname,sender_email) {
#  sht <- tryCatch(read_file_x1(Path,sheetname),
#                  error = function(e) {
#                    #file.copy(Path,'ERROR_FILE')
#                    sht <- data.frame( One=character(),
#                                       Two=character(), 
#                                       Three=character(),
#                                       Four=character(),
#                                       Five=character(),
#                                       Six=character(),
#                                       Seven=character(),
#                                       Eight=character(),
#                                       Nine=character(),
#                                       Ten=character(),
#                                       Eleven=character(),
#                                       Twelve = character())
#                    send_email(
#                      sender_email,
#                      "ERROR IN MANIFEST FILE",
#                      paste("AN ERROR OCCURED ON ATTACHED FILE",a),
#                      Path
#                    )
#                  })
#  
#  if(is.data.frame(sht) && nrow(sht) == 0) {
#    #file.copy(Path,'ERROR_FILE')
#   # send_email(
#   #   sender_email,
#   #   "ERROR IN MANIFEST FILE",
#   #   "PLEASE CHECK THE ATTACHED FILE SEEMS INCORRECT. FIX IT & SEND AGAIN WITH THE SAME NAME TO MUHAMMAD.KHAN@GERRYSDNATA.cOM",
#   #   Path
#   # )
#  }else if(is.data.frame(sht) && (ncol(sht) != 12 || sum(is.na(sht$Flight_No)) > 0 || sum(is.na(sht$Flight_date)) > 0)){
#    #file.copy(Path,'ERROR_FILE')
#    return(sht)
#  }
#}


read_all_files_in_dir <- function(Path, Pattern,sheetname,sender_email) {
  
  list_files <-
    list.files(Path ,
               pattern = Pattern,
               all.files = FALSE,
               full.names = TRUE,
               include.dirs = TRUE,
               recursive = TRUE
    )
  df <- lapply(list_files, read_file_x1,sheetname)
  
  if(length(resend_files) > 0){
    #y <- htmlTable(as.data.frame(resend_files), rnames = FALSE)
    #html_body <- paste0("<p> Please resend below files. </p>", y)
    send_email(
      sender_email,
      paste("ERROR IN MANIFEST FILE",'     ',Sys.time()),
      gsub(".*/","\n",resend_files)
    )
  }
  
  return(df)

}


Valid_AWB <- function(df, col_name) {
  df[[col_name]] <- trimws(df[[col_name]])
  df[[col_name]] <-
    gsub("[!@#$%^&*()_=+/,.?\":;\'{}|<>-]", "", df[[col_name]])
  df[[col_name]] <- gsub(" ", "", df[[col_name]])
  
  df[[col_name]] <- gsub('^(.{3})(.*)$', '\\1-\\2', df[[col_name]])
  
  df <- df[grepl("\\d{3}-[0-9]+", df[[col_name]]), ]
  return(df)
  
}