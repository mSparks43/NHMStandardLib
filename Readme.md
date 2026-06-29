# NHM Standard Library

Standard library for working with NHM datasets.

## Installation

R/Java integration is required, to configure your R installation the simpliest way to acheive this is to run from the R console:

```R

library(rJavaEnv)
rje_consent(provided = TRUE)
java_quick_install(25)
use_java(25)
options(java.parameters = c("-Xmx7g",
                            "-Djdk.xml.maxGeneralEntitySizeLimit=0",
                            "-Djdk.xml.totalEntitySizeLimit=0",
                            "-Djdk.xml.entityExpansionLimit=0"))
library(rJava)

```

To install this library run from the R console:

```R
install.packages("pak")
pak::pak("mSparks43/NHMStandardLib")

```


