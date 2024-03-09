# NHM Stadard R Library
# Helper functions for analysts working with Nation Health Model datasets
# Zem Solutions, all rights reserved.
# use `roxygen2::roxygenise()` to build

## Database functions

#' import an NHM database into R
#'
#' @param dbName the number of workers
#' @return a dataframe containing patient record data in json
#'
#' @export
importDataBase<-function(dbName){
  setwd(dbDir)
  str<-paste("openAccess_NHM_Patients_",dbName,".db",sep = "")
  #connecting file location and r, r now has acces to the info
  connection <- dbConnect(SQLite(), str)
  res <- dbSendQuery(connection, "SELECT * FROM datatable")
  rawData <- dbFetch(res)
  dbClearResult(res)
  jsonStrings <- rawData[,"data"] #za sve redove (prazno pre zareza) uzmi podatke iz kolone "data"
  length(jsonStrings)

  data_table<-tbl(connection,"datatable")
  #we can see first 10 lines of the database in console
  #head(data_table)
  data_table_df<-collect(data_table)
  #we have directly loaded the data into r
  raw_data<-data_table_df[[2]]
  dbDisconnect(connection)
  setwd(baseDir)
  return(raw_data)
}

#' @export
convertIntervention <- function(value,controlName,controlMatch,interventionName,interventionMatch){
  rowIsIntervention=F
  rowIsControl=F
  for(n in 1:nrow(controlMatch)){
    if(controlMatch[n,1]=="starts"&&startsWith(value,controlMatch[n,2]))
      rowIsControl=T
    else if(controlMatch[n,1]=="ends"&&endsWith(value,controlMatch[n,2]))
      rowIsControl=T
  }
  for(n in 1:nrow(interventionMatch)){
    if(interventionMatch[n,1]=="starts"&&startsWith(value,interventionMatch[n,2]))
      rowIsIntervention=T
    else if(interventionMatch[n,1]=="ends"&&endsWith(value,interventionMatch[n,2]))
      rowIsIntervention=T
  }
  if(rowIsControl)
    return(controlName)
  if(rowIsIntervention)
    return(interventionName)
  return(NA)
}

## Excel Document helper functions

#' @export
excel_doc <- function(filename,y=0,sheet=1,col = TRUE){
  print(CONCAT("Reading ",filename))
  return (read_excel(filename, sheet, col_names=col, skip=y))
}

#' @export
startWorkBook <- function(){
  wb<-createWorkbook(type="xlsx")
  TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
  TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"),
           pen=c("BORDER_THIN", "BORDER_THICK"))
  return (wb)
}

#' Convert Excel column letters into numbers
#'
#' @param s A string of letters to convert (e.g. "AA")
#' @return The column number represented by this string
#'
#' @export
SpreadsheetLettersToNumbers <- function(s){
  # Uppercase
  s_upper <- toupper(s)
  # Convert string to a vector of single letters
  s_split <- unlist(strsplit(s_upper, split=""))
  # Convert each letter to the corresponding number
  s_number <- sapply(s_split, function(x) {which(LETTERS == x)})
  # Derive the numeric value associated with each letter
  numbers <- 26^((length(s_number)-1):0)
  # Calculate the column number
  column_number <- sum(s_number * numbers)
  column_number
}

#' @export
putExcel<- function(dFrame,sheet,x = 1,y = 1,nameCols=TRUE){
  createWS <- TRUE
  workSheets<-getSheets(workbook)
  for (sheetName in names(workSheets)){
    if (sheetName == sheet) {
      createWS <- FALSE
    }
  }
  if(createWS){
    #addWorksheet(workbook, sheet)
    print(CONCAT("Create sheet:",sheet))
    createSheet(workbook, sheetName = sheet)
  }
  workSheets<-getSheets(workbook)
  TABLE_ROWNAMES_STYLE <- CellStyle(workbook) + Font(workbook, isBold=TRUE)
  TABLE_COLNAMES_STYLE <- CellStyle(workbook) + Font(workbook, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"),
           pen=c("BORDER_THIN", "BORDER_THICK"))
  #writeData(workbook, sheet, dFrame, startRow = y, startCol = x,colNames=nameCols)
  addDataFrame(dFrame, workSheets[[sheet]], startRow=y, startColumn=x,
               colnamesStyle = TABLE_COLNAMES_STYLE,
               rownamesStyle = TABLE_ROWNAMES_STYLE)
  # Change column width
  setColumnWidth(workSheets[[sheet]], colIndex=c(1:ncol(state.x77)), colWidth=11)
  #setRowHeight(workSheets[[sheet]], rows=c(1), inPoints=22)
}

#' @export
saveExcel<-function(filename){
  setwd(baseDir)
  setwd(outputDir)
  print(CONCAT("Saving Excel file:",baseDir,outputDir,filename))
  saveWorkbook(workbook, file = filename)
  setwd(baseDir)
}


## Helper functions
#' @export
CONCAT <- function (..., sep = "", collapse = NULL, recycle0 = FALSE){
  .Internal(paste(list(...), sep, collapse, recycle0))
}

#' @export
contains <- function(needle,haystack){
  return(grepl(needle, haystack, fixed = TRUE))
}

#' @export
strlen <- function(string){
  return (sum(nchar(string)))
}

#' @export
se_or95_to_sd<-function(se,n,l95,u95){
  if(is.na(l95)||is.na(u95))
    return(se_to_sd(se,n))
  se_local<-(u95-l95)/3.92
  return(se_to_sd(se_local,n))
}

#' @export
sd_or95_to_sd<-function(se,n,l95,u95){
  if(is.na(l95)||is.na(u95))
    return(sd_to_sd(se,n))
  se_local<-(u95-l95)/3.92
  return(se_to_sd(se_local,n))
}

#' @export
se_to_sd<-function(se,n){
  if(is.na(se))
    return(NA)
  return (se*sqrt(n))
}

#' @export
sd_to_sd<-function(sd,n){
  # if(is.na(sd))
  #   return(NA)
  return (sd)
}

#' @export
none_to_sd<-function(sd,n){
  # if(is.na(sd))
  #   return(NA)
  return (5*runif(1))
}

#' @export
sd_cfb<-function(SDb,SDf,Corr){
  i<-SDb^2 + SDf^2 - (2 * Corr * SDb*SDf)
  return (sqrt(i))
}

#' @export
iqr_to_sd<-function(q1,q3,n){
  if(is.na(q1)||is.na(q3)||is.na(n))
    return(NA)
  return((q3 - q1) / (2 * (qnorm((0.75 * n - 0.125) / (n + 0.25)))))

}

#' @export
ci95_to_sd<-function(n,l95,u95){
  if(is.na(l95)||is.na(u95))
    return(NA)
  se_local<-(u95-l95)/3.92
  return(se_to_sd(se_local,n))
}

##Graph helpers

human_numbers <- function(x = NULL, smbl ="", signif = 1){
  humanity <- function(y){

    if (!is.na(y)){
      tn <- round(abs(y) / 1e12, signif)
      b <- round(abs(y) / 1e9, signif)
      m <- round(abs(y) / 1e6, signif)
      k <- round(abs(y) / 1e3, signif)

      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }

      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "k")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"m")
      }else if(tn < 1){
        #paste0 (y_is_positive, smbl, b ,"bn")
        paste0 (y_is_positive, smbl, b ,"mlrd")
      } else {
        paste0 (y_is_positive, smbl,  comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }

  sapply(x,humanity)
}

#' Human readable versions of large numbers GBP
#'

#' @export
human_gbp   <- function(x){human_numbers(x, smbl = "£")}

#' Human readable versions of large numbers USD
#'
#' @export
human_usd   <- function(x){human_numbers(x, smbl = "$")}

#' Human readable versions of large numbers EURO
#'
#' @export
human_euro  <- function(x){human_numbers(x, smbl = "€")}
#' Human readable versions of large numbers no units
#'
#' @export
human_num   <- function(x){human_numbers(x, smbl = "")}
#' Human readable versions of large numbers RSD
#'
#' @export
human_rsd  <- function(x){human_numbers(x, smbl = "RSD ")}
