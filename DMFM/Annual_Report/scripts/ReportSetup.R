# Performs basic set up for running all scripts

# Load necessary R packages
library(tidyverse)
library(odbc)
library(DBI)
library(dbplyr)
library(lubridate)
library(knitr)
library(rmarkdown)
library(ggpubr)
library(patchwork) #Required for arranging multiple plots, more flexible
library(scales)

# Set background variables
ReportStart <- as.Date(paste0(ReportYear, "-01-01"))
ReportEnd <- as.Date(paste0(ReportYear, "-12-31"))
Estuaries <- c("AB")

# Configure chunks
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE
)