
#' @export
mapReduce_map_toFileList<-function(inData){
  thisSize<-length(inData)
  hasData<-T
  resultDataall<-data.frame()
  start<-1
  end<-min(pkg.env$batchSize,length(inData))
  length(inData)
  retVal<-list()
  fileList<-list()
  while(hasData){
    file<-tempfile()
    dataS<-inData[c(start:end)]
    saveRDS(dataS,file)
    message(CONCAT("mapReduce_map create tmpdata ",start," to ",end," of ",thisSize," ",file))
    fileList<-append(fileList,file)
    start<-start+pkg.env$batchSize
    end<-min(end+pkg.env$batchSize,length(inData))
    if(start>=length(inData))
      hasData<-F
  }
  return (fileList)
}

#' @export
mapReduce_map_fromtoFileList<-function(fileList,mapFunction){
  gc()
  retFileList<-list()
  for(i in fileList){

    dataS<-readRDS(i)
    #unlink(i)
    message(CONCAT("mapReduce_map Process ",i," ",length(dataS)))
    #iLV<-parLapply(cl,dataS,fun=mapFunction)
    iLV<-mclapply(dataS, mapFunction,mc.cores = pkg.env$numCores)
    message(CONCAT("mapReduce_map compressList"))
    iretVal<-list(iLV)[[1]]
    #iretVal<-list()
    gc()
    message(CONCAT("mapReduce_map cleaning ",length(iretVal)))
    iretVal<-list_drop_empty(iretVal)
    message(CONCAT("mapReduce_map cleaned ",length(iretVal)))
    gc()
    #retVal<-append(retVal,iretVal)
    message(CONCAT("mapReduce_map now ",length(iretVal)))
    file<-tempfile()
    saveRDS(iretVal,file)
    message(CONCAT("mapReduce_map create new tmpdata from ",i," to ",file))
    retFileList<-append(retFileList,file)
  }
  return (retFileList)
}

#' @export
mapReduce_unlinkFileList<-function(fileList){
  for(i in fileList){
    message(CONCAT("unlink ",i))
    unlink(i)
  }
}
#' @export
mapReduce_merge<-function(fileList){
  retVal<-list()
  message(CONCAT("mapReduce merging map"))
  for(i in fileList){
    dataS<-readRDS(i)
    retVal<-append(retVal,dataS)
    message(CONCAT("mapReduce_map now ",length(retVal)))
  }
  return (retVal)
}
#' Map Reduce Map Function
#'
#' parse a map function over source data
#' @param srcDoc the source document to parse
#' @param mapfunction the function to use, should emit a dataframe, empty if no items
#' @return a list of mapped items
#' @examples
#' emitFunction <- function(row) {
#'   if(is.null(row[['Molecule']]))
#'     return (data.frame())
#'   retVal <- data.frame(molecule=row[['Molecule']],count=1)
#'   return (retVal)
#' }
#'
#' list_data <- mapReduce_map(ndjson_data,emitFunction)
#'
#' @export
mapReduce_map<-function(srcDoc,mapFunction){
  if (is.data.frame(srcDoc)){
    if(nrow(srcDoc)<pkg.env$batchSize){
      inData <- split(srcDoc, 1:nrow(srcDoc))
    }else{
      inDataL <- suppressWarnings({split(srcDoc, 1:(nrow(srcDoc)/pkg.env$batchSize))})
      hasData<-T
      retVal<-list()
      thisSize<-nrow(srcDoc)
      for(i in 1:length(inDataL)){


        message(CONCAT("mapReduce_map Process ",nrow(inDataL[[i]])))
        inData <-split(inDataL[[i]], 1:nrow(inDataL[[i]]))
        message(CONCAT("mapReduce_map split done"))
        tretVal<-list(mclapply(inData, mapFunction,mc.cores = pkg.env$numCores))[[1]]
        retVal<-append(retVal,tretVal)
        message(CONCAT("mapReduce_map has ",length(retVal)," elements"))
        gc()
      }
      return(retVal)
    }
  } else {
    inData <- srcDoc
  }

  message(CONCAT("mapReduce_map Mapping ",length(inData)))
  if(pkg.env$numCores>1){
    cl <- makeForkCluster(pkg.env$numCores)
    setDefaultCluster(cl)
    #retVal<-list(mclapply(inData, mapFunction,mc.cores = pkg.env$numCores))[[1]]
    thisSize<-length(inData)
    hasData<-T
    resultDataall<-data.frame()
    start<-1
    end<-min(pkg.env$batchSize,length(inData))
    length(inData)
    retVal<-list()
    fileList<-list()
    while(hasData){
      file<-tempfile()
      dataS<-inData[c(start:end)]
      saveRDS(dataS,file)
      message(CONCAT("mapReduce_map create tmpdata ",start," to ",end," of ",thisSize," ",file))
      fileList<-append(fileList,file)
      start<-start+pkg.env$batchSize
      end<-min(end+pkg.env$batchSize,length(inData))
      if(start>=length(inData))
        hasData<-F
    }
    gc()
    for(i in fileList){

      dataS<-readRDS(i)#inData[c(start:end)]
      unlink(i)
      message(CONCAT("mapReduce_map Process ",i," ",length(dataS)))
      #iLV<-parLapply(cl,dataS,fun=mapFunction)
      iLV<-mclapply(dataS, mapFunction,mc.cores = pkg.env$numCores)
      message(CONCAT("mapReduce_map compressList"))
      iretVal<-list(iLV)[[1]]
      #iretVal<-list()
      gc()
      message(CONCAT("mapReduce_map cleaning ",length(iretVal)))
      iretVal<-list_drop_empty(iretVal)
      message(CONCAT("mapReduce_map cleaned ",length(iretVal)))
      gc()
      retVal<-append(retVal,iretVal)
      message(CONCAT("mapReduce_map now ",length(retVal)))

    }
    stopCluster(cl)
  } else {
    retVal<-list(mclapply(inData, mapFunction,mc.cores = pkg.env$numCores))[[1]]
    gc()
    message(CONCAT("mapReduce_map cleaning ",length(retVal)))
    retVal<-list_drop_empty(retVal)
    message(CONCAT("mapReduce_map cleaned ",length(retVal)))
    gc()
  }

  return (retVal)
}
#' Map Reduce Map Function for ndjson files
#'
#' parse a map function over source data
#' @param srcDoc the source file connection to parse
#' @param mapfunction the function to use, should emit a dataframe, empty if no items
#' @return a list of mapped items
#' @examples
#' file<-"data/flight_data6.jdat"
#' processRow <- function(row){
#'   if(startsWith(row,"{")){
#'     json_final<-fromJSON(row)
#'     if(!is.null(json_final[["flightData"]])){
#'       timeStamp<-as.numeric(as.POSIXct(strptime(json_final[["time"]], "%Y/%m/%d %H:%M:%S")))
#'       if(timeStamp>=initial_time)
#'         return(data.frame(json_final[["flightData"]],
#'                time=json_final[["time"]],timestamp=((timeStamp-initial_time)/60)))
#'       else
#'         return(data.frame())
#'     }
#'     else
#'       return(data.frame())
#'   } else {
#'     return(data.frame())
#'   }
#' }
#' con = file(file, "r")
#' list_data <- mapReduce_map_ndjson(con,processRow)
#' close(con)
#'
#' @export
mapReduce_map_ndjson<-function(srcDoc,mapFunction){
  return(lapply(readLines(srcDoc, n=-1, warn=FALSE), mapFunction))
}


#' Map Reduce Reduce function for file lists
#'
#' Process a list of mapped dataframes and return a dataframe containing c(key) with c(functions) applied to c(summary_vars)
#' @param fileList a file list from mapReduce_map_fromtoFileList
#' @param key a c("key") containing the column names to group by
#' @param functions a c("functions") to apply to the reduction
#' @param summary_vars a c("variables") to apply the functions
#' @return a dataframe with the result of the reduction step
#' @examples
#' molecules <- mapReduce_reduce(list_data,c("molecule"),c("sum"),c("count"))
#'
#' @export
mapReduce_reduce_fromFilelist<-function(fileList,key, functions, summary_vars){
  resultDataall<-data.frame()
  for(i in fileList){
    keyS<-paste(key,collapse=" ")
    message(CONCAT("mapReduce_reduce key=(",keyS,") Process ",i))
    dataS<-readRDS(i)
    iresultDataall<-mapReduce_reduce(dataS,key,functions,summary_vars,F)
    resultDataall<-rbind(resultDataall,iresultDataall)
    message(CONCAT("mapReduce_reduce key=(",keyS,") has ",nrow(resultDataall)," rows"))
    gc()

  }
  resultDataall<-mapReduce_reduce(iresultDataall,key,functions,summary_vars,F)
  message(CONCAT("mapReduce_reduce DONE key=(",keyS,") has ",nrow(resultDataall)," rows"))
  return(resultDataall)
}

#' Map Reduce Reduce function
#'
#' Process a list of mapped dataframes and return a dataframe containing c(key) with c(functions) applied to c(summary_vars)
#' @param dt_s the list to reduce, result of mapReduce_map
#' @param key a c("key") containing the column names to group by
#' @param functions a c("functions") to apply to the reduction
#' @param summary_vars a c("variables") to apply the functions
#' @return a dataframe with the result of the reduction step
#' @examples
#' molecules <- mapReduce_reduce(list_data,c("molecule"),c("sum"),c("count"))
#'
#' @export
mapReduce_reduce<-function(dt_s,key, functions, summary_vars,doBatch=T){
  if(is.data.frame(dt_s)){

      getRow<-function(x){
        return(data.frame(x))
      }
      dt_s<-mapReduce_map(dt_s,getRow)
  }

  if(doBatch&&length(dt_s)>pkg.env$batchSize && !missing(key) && !missing(functions)&& !missing(summary_vars)){
    thisSize<-length(dt_s)
    hasData<-T
    resultDataall<-data.frame()
    start<-1
    end<-pkg.env$batchSize
    while(hasData){
      keyS<-paste(key,collapse=" ")
      message(CONCAT("mapReduce_reduce key=(",keyS,") Process ",start," to ",end," of ",thisSize))
      dataS<-dt_s[c(start:end)]
      iresultDataall<-mapReduce_reduce(dataS,key,functions,summary_vars,F)
      resultDataall<-rbind(resultDataall,iresultDataall)

      start<-start+pkg.env$batchSize
      end<-min(end+pkg.env$batchSize,length(dt_s))
      if(start>=length(dt_s))
        hasData<-F
      message(CONCAT("mapReduce_reduce key=(",keyS,") has ",nrow(resultDataall)," rows"))
      gc()
    }
    resultDataall<-mapReduce_reduce(iresultDataall,key,functions,summary_vars,F)
    message(CONCAT("mapReduce_reduce DONE key=(",keyS,") has ",nrow(resultDataall)," rows"))
    return(resultDataall)
  }
  if(!pkg.env$registered){
      registerDoParallel(pkg.env$numCores)
      pkg.env$registered <- TRUE
    }
  if(pkg.env$numCores>1){

    mapReducer <- function(x) {
      retVal<- foreach(i=x, .combine=rbind) %dopar% {
        dt_s[[i]]
      }
    }
    bins<-c()
    for (i in seq(1, length(dt_s), ceiling(length(dt_s)/pkg.env$numCores))){
      bin<-data.frame(i:min((i+ceiling(length(dt_s)/pkg.env$numCores))-1,length(dt_s)))
      bins<-append(bins,bin)
    }

    dt_s2<-list( mclapply(bins, mapReducer,mc.cores = pkg.env$numCores))[[1]]

    retVal<- foreach(i=1:length(dt_s2), .combine=rbind) %dopar% {
      dt_s2[[i]]
    }
    if(!missing(key) && !missing(functions)&& !missing(summary_vars)){
      message("reduce")
      nkey = rlang::syms(key)
      summary_exprs <- rlang::parse_exprs(glue::glue('{functions}({summary_vars}, na.rm = TRUE)'))
      names(summary_exprs) <- glue::glue('{functions}_{summary_vars}')
      retVal <- retVal %>% group_by(!!!nkey) %>% summarise(!!!summary_exprs, .groups = 'drop')
      if(length(summary_vars)==length(unique(summary_vars))){
        renames<-append(key,summary_vars)
        names(retVal)<-renames
      }
    }else{
      message("no reduce")
    }
    gc()
    return(retVal)
  } else {
    retVal<- foreach(i=1:length(dt_s), .combine=rbind) %dopar% {
      dt_s[[i]]
    }
    if(!missing(key) && !missing(functions)&& !missing(summary_vars)){
      message("reduce")
      key = rlang::syms(key)
      summary_exprs <- rlang::parse_exprs(glue::glue('{functions}({summary_vars}, na.rm = TRUE)'))
      names(summary_exprs) <- glue::glue('{functions}_{summary_vars}')
      retVal <- retVal %>% group_by(!!!key) %>% summarise(!!!summary_exprs, .groups = 'drop')
      if(length(summary_vars)==length(unique(summary_vars))){
        renames<-append(key,summary_vars)
        names(retVal)<-renames
      }
      gc()
    }else{
      message("no reduce")

    }
    gc()

    return(retVal)
  }
}
#' Map Reduce update number of worker
#'
#' By default, detectCores is used to set the number of workers, use this function to
#' modify this value.
#' @param numWorkers the number of workers
#' @return void
#'
#' @export
mapReduce_numWorkers <- function(numWorkers){
  pkg.env$numCores <- min(c(numWorkers,pkg.env$maxCores))
}

#' Map Reduce set batch size
#'
#' By default, 35,000 is used update this relative to available memory
#' @param batchSize the number of elements to run in a batch
#' @return void
#'
#' @export
mapReduce_setBatchSize <- function(batchSize){
  pkg.env$batchSize<-batchSize
}

