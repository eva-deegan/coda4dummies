### Set up the coda4dummies package

library("usethis")
#install.packages("pak", repos = "https://r-lib.github.io/p/pak/devel/")
library("pak")

# Create a new package -------------------------------------------------
#create_package(getwd()) ## used this to create package-specific folders

# Modify the description ----------------------------------------------
#use_mit_license("Emma Reich")

# Make a Vignette ----------------------------------------------

# Require external dependencies ----------------------------------------------
use_package("dplyr", "Imports")
use_package("utils", "Imports")
use_package("purrr", "Imports")
pak::pkg_deps("fellmk/PostJAGS/postjags")

# Run this to update documentation
devtools::document()
