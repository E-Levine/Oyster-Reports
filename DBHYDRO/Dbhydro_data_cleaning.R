###DBHYRDRO data cleaning
#
##Takes raw data input, cleans, and outputs to combined file based on Estuary (file) and Station (sheet)
#
# Load necessary R packages
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, openxlsx, lubridate,
               readxl,
               install = TRUE)
#
source("DBHYDRO_Functions.R")
#
##Raw data files should be saved as Excel files in the "Local_data" folder using naming schema:
#two-letter estuary code, _, station (with special characters removed), _, DBKey: i.e., SL_S80S_DJ238
#
###Set up parameters
Data_type <- c("Flow") #Current options: "Flow"
Estuary_code <- c("SL") #two-letter site code
Site_code <- c("SL-N") #Is the data attributable to a specific site? If so then include code, otherwise NA
Station <- c("Gordy") #station ID with special characters removed
ID <- c("91295")
Version <- "Adding" #"New" file or sheet started for the station, or "Adding" new data to an existing file
#
#
#
#####Load new data
#
#Check the the file name matches "filename"
(filename <- paste0("Local_data/", Estuary_code, "_", Station, "_", ID, ".xlsx"))
#
Raw_df <- load_raw_data(filename)
#Check if any data has not been checked/revised - 0 rows = good
Raw_df %>% filter(is.na(Revision_Date) | !(Revision_Date > Date))
#
#
##Prepare data for output
Cleaned_df <- Raw_df %>% dplyr::select(-any_of(c("Revision_Date", "Qualifier"))) %>% 
  rename(!!Data_type := 'Data_Value', Data_Station = "Station") %>%
  mutate(Analysis_Date = as.Date(paste(substr(Date, 1, 4), substr(Date, 6, 7), "15", sep = "-"), format = "%Y-%m-%d"),
         Estuary = Estuary_code,
         Site = Site_code, .before = Data_Station)
#Check data makes sense
head(Cleaned_df)
#
#
#
#
#######Write cleaned data to shared file
#Confirm shared file path
(shared_file_path <- paste0("Shared_data/", Estuary_code, "_", Data_type,".xlsx"))
output_shared_data(shared_file_path, Station)
#
#
#
#
####Summarize data if needed
#Can be run after data has been cleaned and added to the shared folder. Does not currently run from local files. 
##Can NOT be run without existing data file.
#Continues from code above or can be run starting here.
(shared_file_path <- paste0("Shared_data/", Estuary_code, "_", Data_type,".xlsx"))
#
##Check all possible sheets and dates available:
date_range_check(shared_file_path)
##Use table to select stations by name and date range:
Summarize_stations <- c("S80S", "S97S", "S49S", "Gordy") #List of all stations
Date_range <- c(as.Date("2024-01-01"), as.Date("2024-12-31")) #Min and max dates inclusive
Site_sum_name <- c("SLESum") #Column name for summary data
#
data_summary_output(Summarize_stations, Date_range, Site_sum_name)
#