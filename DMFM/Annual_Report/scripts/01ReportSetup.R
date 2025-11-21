# Performs basic set up for running all scripts

# Load necessary R packages
library(tidyverse) # Loads a number of very useful packages
library(odbc) # Necessary for loading data from SQL Server
library(DBI) # Necessary for loading data from SQL Server
library(dbplyr) # Necessary for loading data from SQL Server
library(lubridate) # Makes dealing with dates easier
library(knitr) # Allows Dynamic Report generation (knit)
library(rmarkdown) # Necessary for working with RMarkdown files
library(ggpubr) # Increases the flexibility of ggplot2 generated plots
library(patchwork) #Required for arranging multiple plots, more flexible
library(scales) # Improves scales in plot labels
library(plotrix) # needed for standard error calculations
library(glmmTMB) # needed for modeled means
library(DHARMa) # needed for model diagnostics
library(AICcmodavg) # needed for model selection
library(emmeans) # needed to extract modeled means and CI

# Set background variables
ReportStart <- as.Date(paste0(ReportYear, "-01-01"))
ReportEnd <- as.Date(paste0(ReportYear, "-12-31"))
Estuaries <- c("AB")

# Set data ranges
Years_WQ <- seq(2015, ReportYear)
Years_Survey <- seq(2015, ReportYear)
Years_Recruitment <- seq(2015, ReportYear)
Years_OyHealth <- seq(2016, ReportYear)

# Set important lists
lv3 <- c("W", "C", "E")

# Configure chunks
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE
)