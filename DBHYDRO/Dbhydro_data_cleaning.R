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
#
###Set up parameters
Data_type <- c("Flow") #Current options: "Flow"
Estuary_code <- c("CR") #two-letter site code
Site_code <- c("CR") #Is the data attributable to a specific site? If so then include code, otherwise NA
Station <- c("S79") #station ID with special characters removed
ID <- c("DJ237")
Version <- "New" #"New" file or sheet started for the station, or "Adding" new data to an existing file
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
Cleaned_df <- Raw_df %>% dplyr::select(-Revision_Date, -Qualifier) %>% 
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
  # Check if the specified sheet exists
  if (Station %in% excel_sheets(paste0("Shared_data/", Estuary_code, "_", Data_type,".xlsx"))) {
    print(paste0("Sheet exists for station ", Station, "."))
    } else {
      #Load workbook
      wb <- loadWorkbook(paste0("Shared_data/", Estuary_code, "_", Data_type,".xlsx"))
      addWorksheet(wb, Station)
      writeData(wb, Station, Cleaned_df)
      saveWorkbook(wb, paste0("Shared_data/", Estuary_code, "_", Data_type,".xlsx"), overwrite = TRUE)
      print(paste0("Sheet did NOT exist for station ", Station, " so was created."))
      }
  } else {
  # Create a new workbook
  wb <- createWorkbook()
  # Add a worksheet adnd write the data frame to the sheet
  addWorksheet(wb, Station)
  writeData(wb, Station, Cleaned_df)
  # Save the workbook to a file
  saveWorkbook(wb, paste0("Shared_data/", Estuary_code, "_", Data_type, ".xlsx"), overwrite = TRUE)
  print(paste0("Data file did NOT exist so was created. File: ", Estuary_code, "_", Data_type, ".xlsx  Sheet:", Station))
}
#
#