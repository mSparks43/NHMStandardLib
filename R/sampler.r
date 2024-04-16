
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
itt_patient<-function(document,versionDateName){
  inList<-patient_samples[patient_samples$pid==document$ID & patient_samples$versionDateName==versionDateName,]
  if(length(inList>0))
    return(TRUE)
  return(FALSE)
}

#' Get a sample of patient IDs from an NHM database that meet given criteria
#' @param dbDir the directory to find the database
#' @param versionDateName the ID of the patient database
#' @param patient_samples existing patient_samples to append to
#' @param base_itt_patient A function to test if the patient should be sampled
#' @return a dataframe containing patient record versionDateName and IDs
#'
#'@export
getSample<-function(dbDir,versionDateName,patient_samples,base_itt_patient){
  thisSample<-getSampleITTPopulation(dbDir,versionDateName,base_itt_patient)
  set.seed(1234)
  thisSample<-thisSample[sample(nrow(thisSample), sampleSize), ]
  patient_samples<-rbind(patient_samples,thisSample)
  return(patient_samples)

}

#' sample an NHM database for patients that meet given criteria
#' @param dbDir the directory to find the database
#' @param versionDateName the ID of the patient database
#' @param base_itt_patient A function to test if the patient should be sampled
#' @return a dataframe containing patient record versionDateName and IDs
#'
#'@export
sampleData<-function(dbDir,versionDateName,base_itt_patient){
  getdata <- function(x) {
    document<-fromJSON(x)
    if(!itt_patient(document,versionDateName))
      return(data.frame())
    return (data.frame(x))
  }
  raw_data<-importNHMDataBase(dbDir,versionDateName)
  #system.time(dt_s<-list( mclapply(raw_data, getdata,mc.cores = numCores))[[1]])
  mapReduce_map(raw_data,getdata)
  retVal<-mapReduce_reduce(dt_s)
  return (retVal)
}
