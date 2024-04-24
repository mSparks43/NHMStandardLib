
#' @export
setg11nLang<-function(g11n){
  pkg.env$g11n<-g11n
}
getg11nLang<-function(){
  return(pkg.env$g11n)
}

getg11nID<-function(textSearch){
  iRow<-pkg.env$g11n_data[pkg.env$g11n_data$text==textSearch,]
  if(nrow(iRow)==0)
    return(-1)
  return(iRow$id)
}

#' @export
addg11n<-function(langJSON){
  langData<-fromJSON(langJSON)
  cData<-pkg.env$g11n_data
  id<-0
  for(i in names(langData)){
    id<-getg11nID(langData[[i]])
    if(id>0)
      break
  }
  if(id<=0)
    id<-max(nrow(cData),1)
  langs<-names(langData)
  n<-0
  for(i in names(langData)){
    text<-langData[[i]]
    val<-data.frame(id,lang=i,text)
    if(nrow(cData)>0)
      cData<-rbind(cData,val)
    else
      cData<-val
    n<-1
  }
  pkg.env$g11n_data<-unique(cData)
  return()
}
#' @export
getg11n<-function(textSearch){
  id<-getg11nID(textSearch)
  if(id<=0)
    stop(CONCAT("No g11n for ",textSearch))
  iRow<-pkg.env$g11n_data[pkg.env$g11n_data$id==id & pkg.env$g11n_data$lang==pkg.env$g11n,]
  if(nrow(iRow)==0)
    stop(CONCAT("No g11n for ",textSearch,"lang=",pkg.env$g11n))
  return(iRow$text)
}

#' @export
importg11n<-function(file_ndjson){
  v<-lapply(readLines(file_ndjson, n=-1, warn=FALSE), addg11n)

}
