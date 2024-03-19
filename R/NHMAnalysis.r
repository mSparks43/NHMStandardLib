#' @export
getEQ5D_mapFunction <- function(x) {
  document<-fromJSON(x)
  ages<-c(0:document$age+1)
  tests<-document[["Tests"]]
  if(is.na(tests[tests$test=="BASELINE_LDL",]$value[1])){
    ldl<-as.numeric(tests[tests$test=="LDL",]$value[1])
  }
  else{
    ldl<-as.numeric(tests[tests$test=="BASELINE_LDL",]$value[1])
  }
  sex <- document$Sex
  return (data.frame(ages,EQ5D=document$EQ5D,sex,ldl,pid=document$ID))
}
#' @export
getFCEs_mapFunction <- function(x) {
  document<-fromJSON(x)
  fcediags <- document$FCEs$Diag1
  fcedage <- document$FCEs$pAge
  fcealive <- document$FCEs$alive

  date <- document$FCEs$date          # ovo sam dodala

  count_diags <- 0                    # ovo sam dodala
  for (i in 1:length(fcediags)){
    count_diags = count_diags + 1
  }

  num_diags <- count_diags


  sex <- document$Sex
  tests<-document[["Tests"]]
  #ldl<-as.numeric(tests[tests$test=="LDL",]$value[1])
  birthYear<-as.numeric(strsplit(document$FCEs[document$FCEs$Diag1=="Z380",]$date, "/")[[1]][1]) # ovo prvo 1 trebalo bi da je patient, a drugo 1 redni broj datuma u nizu datuma
  deathYear<-as.numeric(strsplit(document$FCEs[document$FCEs$alive==FALSE,]$date, "/")[[1]][1]) #- ovo ne radi, a birthYear radi (verovatno zbog indexa 1 i 1 u [])
  #return (data.frame(fcedage,fcediags,fcealive,sex,ldl,pid=document$ID,birthYear))
  return (data.frame(fcedage,fcediags,fcealive,sex,ldl,pid=document$ID))
  #return (data.frame(fcedage,fcediags,fcealive,sex,ldl,pid=document$ID))
}
