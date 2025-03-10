###DBHYRDRO data cleaning
#
##Takes raw data input, cleans, and outputs to combined file based on Estuary (file) and Station (sheet)
#
# Load necessary R packages
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, openxlsx, lubridate,
               readxl,writexl,
               install = TRUE)
#
#
##Raw data files should be saved as Excel files in the "Local_data" folder using naming schema:
#two-letter estuary code, _, station (with special characters removed), _, DBKey: i.e., SL_S80S_DJ238
#
#
###Set up parameters
Data_type <- c("Flow") #Current options: "Flow"
Estuary_code <- c("SL") #two-letter site code
Site_code <- c("SL-S") #Is the data attributable to a specific site? If so then include code, otherwise NA
Station <- c("S80S") #station ID with special characters removed
ID <- c("DJ238")
Version <- "New" #"New" file started for the station, or "Adding" new data to an existing file
#
#
#
###Load new data
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
Cleaned_df <- Raw_df %>% dplyr::select(-Revision_Date) %>% 
  rename(!!Data_type := 'Data_Value', Data_Station = "Station") %>%
  mutate(Analysis_Date = as.Date(paste(substr(Date, 1, 4), substr(Date, 6, 7), "15", sep = "-"), format = "%Y-%m-%d"),
         Estuary = Estuary_code,
         Site = Site_code, .before = Data_Station)
#Check data makes sense
head(Cleaned_df)
#
#
#Write data to file based on if New data or adding to existing data
if (file.exists(paste0("Shared_data/", Estuary_code, "_", Data_type,".xlsx"))) {
  print("Data file exists.")
  #sheets <- excel_sheets(file_name)
  # Check if the specified sheet exists
  if (Station %in% excel_sheets(paste0("Shared_data/", Estuary_code, "_", Data_type,".xlsx"))) {
    print(paste0("Sheet exists for station ", Station, "."))
  } else {
    print(paste0("Sheet does NOT exist for station ", Station, "."))
  }
} else {
  # Create a new workbook
  wb <- createWorkbook()
  # Add a worksheet adnd write the data frame to the sheet
  addWorksheet(wb, Station)
  writeData(wb, Station, Cleaned_df)
  # Save the workbook to a file
  saveWorkbook(wb, paste0("Shared_data/", Estuary_code, "_", Data_type, ".xlsx"), overwrite = TRUE)
  print(paste0("Data file did NOT exist and was created. File: ", Estuary_code, "_", Data_type, ".xlsx  Sheet:", Station))
}
#
#
####Functions####
#
##Format raw data file
load_raw_data <- function(file_name){
    #Read the Excel file
    t <- readWorkbook(file_name, sheet = 1, detectDates = TRUE)
    #Determine where to start data based on empty row
    start_row <- which(t[[1]] == "Station")[1]
    if(!is.na(start_row)){
      Raw_data <- t[start_row:nrow(t),]
      } else {
        Raw_data <- t
        }
    #Promote first row to column names, remove columns of NAs, make sure data is only for desired DBKey
    Raw_data_t <- Raw_data %>% slice(-1) %>% setNames(unlist(Raw_data[1,])) %>% 
      dplyr::select(where(~ !all(is.na(.)))) %>% filter(DBKEY == ID) %>% 
      rename(Date = 'Daily Date', Data_Value = 'Data Value', Revision_Date = 'Revision Date') %>%
      mutate(Date = as.Date(paste(substr(Date, 6, 7), substr(Date, 9, 10), substr(Date,1,4), sep = "/"), format = "%m/%d/%Y"),
             Data_Value = as.numeric(Data_Value),
             Revision_Date = as.Date(paste(substr(Revision_Date, 6, 7), substr(Revision_Date, 9, 10), substr(Revision_Date,1,4), sep = "/"), format = "%m/%d/%Y"))
    #
  return(Raw_data_t)
}
#
#
#
###
#
#
working_raw_data <- function(Version_type){
  if(Version_type == "New"){
    #Read the Excel file
    t <- readWorkbook(paste0("Local_data/", Estuary_code, "_", Station, "_", ID, ".xlsx"), sheet = 1, detectDates = TRUE)
    #Determine where to start data based on empty row
    start_row <- which(t[[1]] == "Station")[1]
    if(!is.na(start_row)){
      Raw_data <- t[start_row:nrow(t),]
    } else {
      Raw_data <- t
    }
    #Promote first row to column names, remove columns of NAs, make sure data is only for desired DBKey
    Raw_data <- Raw_data %>% slice(-1) %>% setNames(unlist(Raw_data[1,])) %>% 
      dplyr::select(where(~ !all(is.na(.)))) %>% filter(DBKEY == ID) 
    #
  } else {
    Raw_data <- c("No data found.")
  }
  return(Raw_data)
}
