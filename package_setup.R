### Set up the coda4dummies package

library("usethis")

# Create a new package -------------------------------------------------
#create_package(getwd()) ## used this to create package-specific folders

# Modify the description ----------------------------------------------
#use_mit_license("Emma Reich")

# Require external dependencies ----------------------------------------------
use_package("dplyr", "Imports")
