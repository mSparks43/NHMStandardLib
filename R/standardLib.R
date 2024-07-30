# NHM Stadard R Library
# Helper functions for analysts working with Nation Health Model datasets
# Zem Solutions, all rights reserved.
# use `rm(list=ls());roxygen2::roxygenise()` to build


pkg.env <- new.env()
## base Functions

pkg.env$has_samples <- FALSE
pkg.env$patient_samples<-data.frame()
pkg.env <- new.env()
pkg.env$g11n<-"sr"
pkg.env$g11n_data<-data.frame()
if(Sys.info()["sysname"][1]=="Linux" || Sys.info()["sysname"][1]=="Darwin") {
  pkg.env$numCores <- detectCores()
}else {
  pkg.env$numCores <- 1
}
pkg.env$maxCores <- pkg.env$numCores
pkg.env$registered <- FALSE
#' get standard folder names for current working directory
#' @export
folderInit <- function(){
  baseDir <<- getwd()
  dbDir<<- "../databases"
  graphDir<<-"graphs/"
  graphSubDir<<-""
  outputDir<<-"output/"
  dir.create(file.path(baseDir, graphDir), showWarnings = FALSE)
  dir.create(file.path(baseDir, outputDir), showWarnings = FALSE)
}

## Database functions

#' import an NHM database into R
#' @param dbDir the directory to find the database
#' @param dbName the ID of the patient database
#' @return a dataframe containing patient record data in json
#'
#' @export
importNHMDataBase<-function(dbDir,dbName){
  baseDir <- getwd()
  setwd(dbDir)
  str<-paste("openAccess_NHM_Patients_",dbName,".db",sep = "")
  #connecting file location and r, r now has acces to the info
  connection <- dbConnect(SQLite(), str)
  setwd(baseDir)
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

#' sample normal distribution with min and max
#'
#' @param n number of samples
#' @param mean mean of samples
#' @param sd Standard Deviation of samples
#' @param min min of samples
#' @param max max of samples
#' @return Samples with given parameters
#'
#' @export
rtruncnorm <- function(n, mean, sd, min = -Inf, max = Inf){
  qnorm(runif(n, pnorm(min, mean, sd), pnorm(max, mean, sd)), mean, sd)
}

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

#' @export
rgbeta <- function(n=1000, mean=0.6, var=0.2, min = -0.6125, max = 1, a=NULL, b=NULL)
  {
    dmin <- mean - min
    dmax <- max - mean

    if (dmin <= 0 || dmax <= 0)
    {
      stop(paste("mean must be between min =", min, "and max =", max))
    }

    if (var >= dmin * dmax)
    {
      stop(paste("var must be less than (mean - min) * (max - mean) =", dmin * dmax))
    }

    # mean and variance of the standard beta distributed variable
    mx <- (mean - min) / (max - min)
    vx <- var / (max - min)^2

    # find the corresponding alpha-beta parameterization
    #a <- 20
    #b <- 2
    if(is.null(a))
      a <- ((1 - mx) / vx - 1 / mx) * mx^2
    if(is.null(b))
      b <- a * (1 / mx - 1)

    print(paste("Alpha parameter:", a))
    print(paste("Beta parameter:", b))

    # generate standard beta observations and transform
    x <- rbeta(n, a, b)
    y <- (max - min) * x + min

    return(y)
  }
##Graph helpers



human_numbers <- function(x = NULL, smbl ="", signif = 1){
  if(pkg.env$g11n=="sr")
    return(human_numbers_sr(x,smbl,signif))
  else if(pkg.env$g11n=="en")
    return(human_numbers_en(x,smbl,signif))
  else
    stop("unknown g11n")
}
human_numbers_sr <- function(x = NULL, smbl ="", signif = 1){
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
        paste0 (y_is_positive, smbl, format(k, big.mark=".",decimal.mark=",")  , " hilj.")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, format(m, big.mark=".",decimal.mark=",") ," mil.")
      }else if(tn < 1){
        #paste0 (y_is_positive, smbl, b ,"bn")
        paste0 (y_is_positive, smbl, format(b, big.mark=".",decimal.mark=",") ," mlrd.")
      } else {
        paste0 (y_is_positive, smbl,  format(tn, big.mark=".",decimal.mark=","), "tn.")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }

  sapply(x,humanity)
}
human_numbers_en <- function(x = NULL, smbl ="", signif = 1){
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
        paste0 (y_is_positive, smbl, b ,"bn")
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

#' Create a plot with multiple lines
#' @examples
#' d<-data.frame()
#' d<-rbind(d,data.frame(age=c(18:100),Legend="male",weight=rnorm(83,100,10)))
#' d<-rbind(d,data.frame(age=c(18:100),Legend="female",weight=rnorm(83,80,5)))
#' multilinePlot(d,"Random Weight Graph","Age","Weight(kg)",age,weight)
#'
#' @export
multilinePlot <- function(dataSrc,t_title,xlabel,ylabel,xdataName,ydataName,yUnits="",seqB=0,seqE=110,seqBy=10){
  dataSrc$Legend<-getg11nSafeVector(dataSrc$Legend)
  ggplot(data = dataSrc, aes(x={{xdataName}}, y = {{ydataName}},color=Legend, linetype = Legend)) +
    geom_line() + geom_point()+
    scale_x_continuous(breaks = seq(seqB, seqE, by = seqBy)) +
    scale_color_manual(values=nhmDefaultColors()) +
    scale_y_continuous(labels = human_num) +
    labs(x=getg11nSafe(xlabel), y=getg11nSafe(ylabel), title=getg11nSafe(t_title)) +
    theme(text = element_text(size = 20))
}

#' create a barchartPlot
#'
#' @param dataSrc dataframe data source
#' @param t_title graph title
#' @param xlabel graph x axis label
#' @param ylabel graph y axis label
#' @param xdataName column name to use for the X axis
#' @param ydataName column name to use for the T axis
#' @return a dataframe containing patient record data in json
#' @export
barchartPlot <- function(dataSrc,t_title,xlabel,ylabel,xdataName,ydataName){
  ggplot(data= dataSrc, aes(x={{xdataName}}, y={{ydataName}}, fill=Legend)) +
    geom_bar(stat='identity', position='dodge') +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+
    scale_fill_manual(values=nhmDefaultColors()) +
    labs(x=xlabel, y=ylabel, title=t_title) +
    scale_y_continuous(labels = human_numbers)
}

#' @export
#'
writePlot <- function(fileName,plotFunction,use.print=TRUE,aspectRatio=0.5){
  folderInit()
  setwd(graphDir)
  savename<-paste(fileName,".png")
  png(savename,width=300,height=(300*aspectRatio), pointsize=20, units = "mm", res = 300)
  if(use.print)
    print(plotFunction)
  else
    plotFunction
  dev.off()
  setwd(baseDir)
}
