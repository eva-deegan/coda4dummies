# coda4dummies

Functions to summarize inconvenient rjags or jagsUI coda outputs. Especially helpful when your coda summary contains timeseries variables of different lengths (e.g., when calculating posteriors for variables at daily, weekly, and seasonal timescales within the same model framework). The main function of this package is to streamline the model fitting/ output summarizing process with the least amount of thinking possible. Many functions included here are wrapper functions around functions from the postjags package developed by Michael Fell (https://github.com/fellmk/PostJAGS).

To install the package add the following code to your R script:

```{r}
install.packages("devtools")\
library(devtools)
```

First download Mike Fell's postjags package, which is a dependency:  
```{r}
devtools::install_github("fellmk/PostJAGS/postjags")\
library(postjags)
```

Then download the coda4dummies package:  
```{r}
devtools::install_github("egreich/coda4dummies", build_vignettes = TRUE)\
library(coda4dummies)
```

Note that you will have to load postjags with library(postjags) everytime you use coda4dummies.
