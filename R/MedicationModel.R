#' @export Medication
Medication<-setRefClass("Medication", fields = list(INN = "character",DDD="numeric",ATC="character",getDailyDose="function",getAnnualDose="function"),
                        methods = list(
                          getDailyDose = function(pData) {return (NA)},
                          getAnnualDose = function(pData) {return (DDD*356)},
                          getDDD = function() {return (DDD)},
                          getATC = function() {return (ATC)}
                        )
)
#' @export MedicationDatabase
MedicationDatabase <- setRefClass("MedicationDatabase", fields = list(medications = "vector",innIndices="list",numMedications="numeric"),
                                  methods = list(
                                    addMedication = function(med) {
                                      medications <<- append(medications,med)
                                      numMedications<<-length(medications)
                                      innIndices[med$INN]<<-numMedications
                                      print(CONCAT("numMedications is now =",numMedications))
                                    },
                                    getByINN = function(inn) {
                                      medIndex<-innIndices[[inn]]
                                      return (medications[[medIndex]])
                                    }
                                  )
)

#' @export
MedicationDatabase_new<-function(){
  return (MedicationDatabase$new())
}

#' @export
getAnnualDosingSet<-function(currentDosing,discontinuation,dayNow,endDay,seperation,usedose){
  d<-dayNow
  dv<-c()
  while(d<endDay){     #thisDiscontinueDay pokupi podatak koji se skladišti u objektu end_1_56 (i drugim) koji se dobija kad dole u f-ju applydose() unesemo koju weibull krivu hocemo da koristimo - ona nam daje dan discontinuation-a
    dv<-append(dv,d)
    d<-d+seperation
  }
  if(!is.null(dv))
    currentDosing<-rbind(currentDosing,data.frame(day=dv,dose=usedose,units="mg",discontinuation))
  return(currentDosing)
}
