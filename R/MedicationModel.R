
#' @export
Medication<-setRefClass("Medication", fields = list(INN = "character",DDD="numeric",ATC="character"),
                        methods = list(
                          getDailyDose = function(pData) {return (NA)},
                          getDDD = function() {return (DDD)},
                          getATC = function() {return (ATC)}
                        )
)
#' @export
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
