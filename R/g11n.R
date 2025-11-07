
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
  if(nrow(iRow)>1)
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
getg11nSafeVector<-function(vectortextSearch){
  if(!is.vector(vectortextSearch))
    stop("not a vector")
  for(i in 1:length(vectortextSearch)){
    vectortextSearch[i]<-getg11nSafe(vectortextSearch[i])
  }
  return(vectortextSearch)
}
#' @export
getg11nSafe<-function(textSearch){
  #print(paste0("search ",textSearch))
  id<-getg11nID(textSearch)
  if(is.na(id)|| id<=0)
    return(textSearch)
  iRow<-pkg.env$g11n_data[pkg.env$g11n_data$id==id & pkg.env$g11n_data$lang==pkg.env$g11n,]
  if(nrow(iRow)==0)
    return(textSearch)
  return(iRow$text)
}
#' @export
g11n_numbers <- function(x = NULL, signif = 1, smbl =""){
  if(is.null(x))
    return(NULL)
  if(all(is.numeric(x))){
    if(pkg.env$g11n=="sr"){
      rV<-format(round(x,digits=signif),big.mark=".",decimal.mark=",")
      return(rV)
    }
    else if(pkg.env$g11n=="en")
    {
      return(format(round(x,digits=signif),big.mark=",",decimal.mark="."))
    }
    else if(pkg.env$g11n=="pl"){
      return(format(round(x,digits=signif),big.mark=" ",decimal.mark=","))
    }
    else
      stop("unknown g11n")
  }
  x<-x%>%mutate_if(can_numeric, as.numeric)
  x<-x%>%mutate_if(is.numeric, round,digits=signif)
  x<-x%>%mutate_if(not_can_numeric, getg11nSafeVector)
  if(pkg.env$g11n=="sr"){
    x<-x%>%mutate_if(is.numeric, format,big.mark=".",decimal.mark=",")
  }
  else if(pkg.env$g11n=="en")
  {
    x<-x%>%mutate_if(is.numeric, format,big.mark=",",decimal.mark=".")
  }
  else if(pkg.env$g11n=="pl"){
    x<-x%>%mutate_if(is.numeric, format,big.mark=" ",decimal.mark=",")
  }
  else
    stop("unknown g11n")
  names(x)<-str_replace_all(names(x),"_"," ")
  names(x)<-str_replace_all(names(x),"\\."," ")
  names(x)<-getg11nSafeVector(names(x))
  return(x)
}

#' @export
importg11n<-function(file_ndjson){
  pkg.env$g11n_data<-data.frame()
  v<-lapply(readLines(file_ndjson, n=-1, warn=FALSE), addg11n)

}
#' @export
importxlg11n<-function(excel_filename){
  tTable <- read_excel(excel_filename,sheet=1)
  for(i in 1:nrow(tTable)){
    addg11n(toJSON(as.list(tTable[i,])))
  }
}
