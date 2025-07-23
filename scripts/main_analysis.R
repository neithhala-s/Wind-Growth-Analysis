# main_analysis.R
# This is the master script to run the entire analysis.
# It sets up the R environment, loads necessary libraries, defines global variables,
# creates output directories for plots and tables, and then sources all
# individual analysis scripts in the correct order to execute the full workflow.
# This script ensures reproducibility and a clear execution flow for the project.


# ---  Setting up Environment and Global Variables ---

# Load common libraries required across multiple scripts
library(tidyverse)
library(readxl)
library(fixest)
library(sandwich) # For vcov in fixest
library(lmtest)   # For coeftest if needed, though fixest summary often suffices
library(extrafont) # For custom font
library(broom)    # For tidy() method
library(writexl)  # For writing excel files
library(ggplot2)  # For advanced plotting
library(dplyr)    # Core data manipulation
library(tseries)  # For time series functions if needed
library(car)      # For companion functions if needed
library(stringr)  # For string manipulation
library(tidyr)    # For pivoting data

# Define global font for plots
my_font <- "Times New Roman"

# Define the intervention year for policy analysis
intervention_year <- 2017

# --- Creating Output Directories ---

# Define paths for output directories
plots_dir <- "output/plots"
tables_dir <- "output/tables"

# Create directories if they don't exist
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
  message(paste0("Created directory: ", plots_dir))
} else {
  message(paste0("Directory already exists: ", plots_dir))
}

if (!dir.exists(tables_dir)) {
  dir.create(tables_dir, recursive = TRUE)
  message(paste0("Created directory: ", tables_dir))
} else {
  message(paste0("Directory already exists: ", tables_dir))
}

# --- Sourcing Individual Analysis Scripts ---

message("Starting data loading and cleaning...")
source("scripts/01_data_loading_and_cleaning.R")

message("Performing EDA and CAGR calculations...")
source("scripts/02_eda_and_cagr.R")

message("Running ITS models (National Level)...")
source("scripts/03_its_modeling.R")

message("Performing State-Level Analysis and Plotting...")
source("scripts/04_state_level_analysis.R")

message("Running Event Study Models...")
# Setting global graphics parameters for base R plots
par(family = my_font, mar = c(6, 6, 4, 12) + 0.1 )
source("scripts/05_event_study_modeling.R")

message("All analysis scripts executed successfully!")
