
Medication<-setRefClass("Medication", fields = list(INN = "character",DDD="numeric",ATC="character",getDailyDose="function",getAnnualDose="function"),
                        methods = list(
                          getDailyDose = function(pData) {return (NA)},
                          getAnnualDose = function(pData) {return (DDD*356)},
                          getDDD = function() {return (DDD)},
                          getATC = function() {return (ATC)}
                        )
)

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
