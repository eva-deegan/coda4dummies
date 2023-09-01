### Set up the coda4dummies package

library("usethis")
library("devtools")

# Create a new package -------------------------------------------------
#create_package(getwd()) ## used this to create package-specific folders

# Modify the description ----------------------------------------------
#use_mit_license("Emma Reich")

# Make a Vignette ----------------------------------------------
#use_vignette("how-to-coda4dummies", "coda4dummies")
devtools::build_vignettes()
devtools::load_all()

#Require external dependencies ----------------------------------------------
#use_package("dplyr", "Imports") use_package("utils", "Imports")
#use_package("purrr", "Imports") use_package("tibble", "Imports")
#use_package("tidyr", "Imports")


# Run this to update documentation
devtools::document()

