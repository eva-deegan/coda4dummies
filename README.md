# coda4dummies

Functions to summarize inconvenient rjags coda outputs. Especially helpful when your coda summary contains timeseries variables of different lengths (e.g., when calculating uncertainties for variables at daily, weekly, and seasonal timescales within the same model framework).

To install the package add the following code to your R script:

install.packages("devtools")\
library(devtools)

First download Mike Fell's postjags package, which is a dependency: devtools::install_github("fellmk/PostJAGS/postjags")\
library(postjags)

Then download the coda4dummies package: devtools::install_github("egreich/coda4dummies")\
library(coda4dummies)

Note that you will have to load postjags with library(postjags) everytime you use coda4dummies.
