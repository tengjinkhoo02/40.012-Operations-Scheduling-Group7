# List of required packages
required_packages <- c("shiny", "shinyjs", "dplyr", "readr", "readxl", "lubridate", "DT", "ggplot2", "gurobi")

# Function to check and install missing packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Check and install missing packages
for (pkg in required_packages) {
  install_if_missing(pkg)
}

# Load the packages
lapply(required_packages, library, character.only = TRUE)

# Specific installation for gurobi, if not available from CRAN
if (!require("gurobi", character.only = TRUE)) {
  install.packages("gurobi", repos = "https://cloud.r-project.org/")
}

# Load gurobi library
library(gurobi)

# Rest of your Shiny app code
library(shiny)
library(shinyjs)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(DT)
library(ggplot2)
library(gurobi)

# Your Shiny app code goes here...
