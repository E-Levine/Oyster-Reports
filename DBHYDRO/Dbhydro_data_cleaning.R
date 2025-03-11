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
Estuary_code <- c("LX") #two-letter site code
Site_code <- c("LX") #Is the data attributable to a specific site? If so then include code, otherwise NA
Station <- c("LOX") #station ID with special characters removed
ID <- c("00295")
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
#Continues from code above or can be run starting here.
(shared_file_path <- paste0("Shared_data/", Estuary_code, "_", Data_type,".xlsx"))
#
##Check all possible sheets and dates available:
date_range_check(shared_file_path)
##Use table to select stations by name and date range:
Summarize_stations <- c("LOX", "S46S") #List of all stations
Date_range <- c(as.Date("2023-01-01"), as.Date("2023-12-31")) #Min and max dates inclusive
Site_sum_name <- c("LOXSum") #Column name for summary data
#
#
## Read all sheets into a list of data frames and name sheets, name list elements with sheet names
sheets_data <- lapply(Summarize_stations, function(sheet) {
  read.xlsx(shared_file_path, sheet = sheet) %>% 
    mutate(Analysis_Date = as.Date(Analysis_Date, origin = "1899-12-30"), Date = as.Date(Date, origin = "1899-12-30")) %>%
    filter(Date >= Date_range[1] & Date <= Date_range[2]) %>% dplyr::select(-Site, -DBKEY, -Data_Station)
})
# Rename the Date_Type column to include the sheet name for clarity
sheets_data <- lapply(seq_along(sheets_data), function(i) {
  df <- sheets_data[[i]]
  sheet_name <- Summarize_stations[i]
  # Check if the required columns exist
  if ("Analysis_Date" %in% names(df) && Data_type %in% names(df)) {
    # Rename the Measurements column to include the sheet name
    colnames(df)[which(names(df) == Data_type)] <- paste(sheet_name, Data_type, sep = "_")
  }
  return(df)
})
names(sheets_data) <- Summarize_stations
# Merge all data frames
merged_data <- Reduce(function(x, y) merge(x, y, by = c("Analysis_Date", "Estuary", "Date"), all = TRUE), sheets_data)
Summarized_data <- merged_data %>% mutate(SUMM = rowSums(select(., 4:ncol(.)), na.rm = TRUE)) %>%
  rename(!!Site_sum_name := SUMM)
#Output/save summary data - if exists appened, if doesn't add new sheet
