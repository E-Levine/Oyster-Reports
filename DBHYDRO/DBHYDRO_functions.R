##Functions used for DBHYDRO data
##
####Data_cleaning####
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
    dplyr::select(where(~ !all(is.na(.))), 'Data Value') %>% filter(DBKEY == ID) %>% 
    rename(Date = 'Daily Date', Data_Value = 'Data Value', Revision_Date = 'Revision Date') %>%
    mutate(Date = as.Date(paste(substr(Date, 6, 7), substr(Date, 9, 10), substr(Date,1,4), sep = "/"), format = "%m/%d/%Y"),
           Data_Value = as.numeric(Data_Value),
           Revision_Date = as.Date(paste(substr(Revision_Date, 6, 7), substr(Revision_Date, 9, 10), substr(Revision_Date,1,4), sep = "/"), format = "%m/%d/%Y"))
  #
  return(Raw_data_t)
}
#
##Write data if new, append data if additional data for existing file/sheet
output_shared_data <- function(Shared_file, sheet_name){
  if(exists("wb")){rm(wb)}
  #Write data to file based on if New data or adding to existing data
  if (file.exists(Shared_file)) {
    print("Data file exists.")
    # Check if the specified sheet exists
    if (sheet_name %in% excel_sheets(Shared_file)) {
      #Load workbook
      wb <- loadWorkbook(Shared_file)
      # Get the existing data from the specified sheet, add new data, keep only newest data if duplicates, and write back to same sheet
      existing_data <- read.xlsx(Shared_file, sheet = sheet_name) %>% 
        mutate(Age = "Older", Analysis_Date = as.Date(Analysis_Date, origin = "1899-12-30"), Date = as.Date(Date, origin = "1899-12-30"))
      combined_data <- rbind(existing_data, Cleaned_df %>% mutate(Age = "Newer")) %>% group_by(Date) %>% arrange(Date, Age) %>% slice(1)
      combined_data <- combined_data %>% dplyr::select(-Age) %>% arrange(Date)
      writeData(wb, sheet = sheet_name, combined_data)
      # Save the workbook
      saveWorkbook(wb, Shared_file, overwrite = TRUE)
      print(paste0("Sheet exists for station ", sheet_name, ". New data was appended to sheet."))
    } else {
      #Load workbook, add worksheet and write data to sheet
      wb <- loadWorkbook(Shared_file)
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, Cleaned_df)
      saveWorkbook(wb, Shared_file, overwrite = TRUE)
      print(paste0("Sheet did NOT exist for station ", sheet_name, " so was created."))
    }
  } else {
    # Create a new workbook, add a worksheet and write the data to sheet
    wb <- createWorkbook()
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, Cleaned_df)
    # Save the workbook to a file
    saveWorkbook(wb, Shared_file, overwrite = TRUE)
    print(paste0("Data file did NOT exist so was created. File: ", Estuary_code, "_", Data_type, ".xlsx  Sheet:", sheet_name))
  }
}