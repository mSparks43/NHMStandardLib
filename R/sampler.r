
getSampleITTPopulation<-function(dbDir,versionDateName,base_itt_patient){
  getPatient <- function(x) {
    document<-fromJSON(x)
    if(!base_itt_patient(document))
      return(data.frame())
    pid<-document$ID
    retVal<-data.frame(versionDateName,pid)
    return (retVal)
  }


  raw_data<-importNHMDataBase(dbDir,versionDateName)
  #system.time(dt_s<-list( mclapply(raw_data, getPatient,mc.cores = numCores))[[1]])
  dt_s<-mapReduce_map(raw_data,getPatient)
  retVal<-mapReduce_reduce(dt_s)
  return (retVal)
}

#' Initialise sampling functions
#'@export
initNHMSampling<-function(useSampling=FALSE){
  pkg.env$has_samples <- useSampling
  pkg.env$patient_samples<-data.frame()
}
#' Get a sample of patient IDs from an NHM database that meet given criteria
#' @param dbDir the directory to find the database
#' @param versionDateName the ID of the patient database
#' @param base_itt_patient A function to test if the patient should be sampled
#' @return a dataframe containing patient record versionDateName and IDs

#'@export
getNHMSampleIDs<-function(dbDir,versionDateName,base_itt_patient,sampleSize){
  if(!pkg.env$has_samples)
    initNHMSampling(useSampling=TRUE)
  thisSample<-getSampleITTPopulation(dbDir,versionDateName,base_itt_patient)
  set.seed(1234)
  if(nrow(thisSample)<sampleSize)
    stop("sampleSize > sample")
  thisSample<-thisSample[sample(nrow(thisSample), sampleSize), ]
  gc()
  pkg.env$patient_samples<-rbind(pkg.env$patient_samples,thisSample)
  return(pkg.env$patient_samples)

}

#' sample an NHM database for patients that meet given criteria
#' @param dbDir the directory to find the database
#' @param versionDateName the ID of the patient database
#' @return a dataframe containing patient record versionDateName and IDs
#'
#'@export
importNHMDataBaseSampleData<-function(dbDir,versionDateName){
  getdata <- function(x) {
    document<-fromJSON(x)
    if(!itt_patient(document,versionDateName))
      return(data.frame())
    return (x)
  }
  itt_patient<-function(document,versionDateName){
    inList<-pkg.env$patient_samples[pkg.env$patient_samples$pid==document$ID & pkg.env$patient_samples$versionDateName==versionDateName,]
    if(length(inList>0))
      return(TRUE)
    return(FALSE)
  }
  raw_data<-importNHMDataBase(dbDir,versionDateName)
  #system.time(dt_s<-list( mclapply(raw_data, getdata,mc.cores = numCores))[[1]])
  dt_s<-mapReduce_map(raw_data,getdata)
  #retVal<-mapReduce_reduce(dt_s)
  return (dt_s)
}
