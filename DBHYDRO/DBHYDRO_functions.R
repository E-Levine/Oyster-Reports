##Functions used for DBHYDRO data
##
####Data_cleaning####
#
##Format raw data file
# file_name to load
# ID of logger
load_raw_data <- function(file_name, ID){
  
  if(file.exists(file_name)){
    print("File found. Loading data from Excel file.")
    #Read the Excel file
    t <- readWorkbook(file_name, sheet = 1, detectDates = TRUE)
  } else if(file.exists(sub(".xlsx", ".csv", file_name))){
    print("File found. Loading data from CSV file.")
    #If no Excel file check for CSV
    t <- read.csv(sub(".xlsx", ".csv", file_name), 
                  colClasses = c("TIMESERIESID" = "character"))
  } else {
    print("File not found. Check file name.")
  }
  
  #Determine where to start data based on empty row
  start_row <- which(t[[1]] == "Station")[1]
  if(!is.na(start_row)){
    Raw_data <- t[start_row:nrow(t),]
  } else {
    Raw_data <- t
  }
  #Promote first row to column names if needed by checking first row, column value
  first_cell <- t[1,1]
  if(first_cell %in% c("DBKEY", "TIMESTAMPID", "TIMESERIESID")){
    Raw_data <- Raw_data %>% slice(-1) %>% setNames(unlist(Raw_data[1,]))
  } else {
    Raw_data <- Raw_data
  }
  
  #Make sure any 'logi' columns are converted to type 'text'
  for (col in names(Raw_data)) {
    if (is.logical(Raw_data[[col]])) {
      Raw_data[[col]] <- as.character(Raw_data[[col]])
    }
  }
  #Check for times in data
  parsed_datetime <- parse_date_time(Raw_data$TIMESTAMP, orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", "ymd", "mdy"))
  has_time <- any(hour(parsed_datetime) != 0 | minute(parsed_datetime) != 0 | second(parsed_datetime) != 0, na.rm = TRUE)
  #
  #Remove columns of NAs, make sure data is only for desired DBKey
  Raw_data_t <- Raw_data %>% 
    # Select all columns that have some data (not all NA) and necessary columns for summarizing
    dplyr::select(where(~ !all(is.na(.))), any_of(c('Data Value', 'VALUE', "Revision Date", "REVISION_DATE"))) %>% 
    # Remove rows without station or dat info
    filter(if_any(any_of(c("DBKEY", "TIMESERIESID")), ~ . == ID)) %>% 
    # Rename columns
    rename(Date = any_of(c("'Daily Date'", "TIMESTAMP")), 
           Data_Value = any_of(c("Data Value", "VALUE")), 
           Revision_Date = any_of(c("Revision Date", "REVISION_DATE"))) %>%
    # Read dates and extract any timestamps, make sure measurements are numeric
    mutate(Full_DateTime = parse_date_time(Date, orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", "ymd", "mdy")),
           Date = as.Date(Full_DateTime),
           Time = if (has_time) format(Full_DateTime, "%H:%M:%S") else NA_character_,
           Data_Value = as.numeric(Data_Value),
           Revision_Date = as.Date(parse_date_time(Revision_Date, orders = c("ymd HM", "mdy HM")))) %>%
    # Remove the temporary datetime column, and drop Time if it wasn't needed
    dplyr::select(-Full_DateTime, -if (!has_time) "Time" else NULL)
  #
  if (has_time) {
    message("Time found in Date information. Separate column created for time information.")
  }
  #
  return(Raw_data_t)
}
#
#
#
####Output of cleaned data
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
        mutate(Age = "Older", 
               Analysis_Date = as.Date(Analysis_Date, origin = "1899-12-30"), 
               Date = as.Date(Date, origin = "1899-12-30"))
      
      combined_data <- rbind(existing_data, 
                             Cleaned_df %>% 
                               mutate(Age = "Newer")) %>% 
        group_by(Date) %>% 
        arrange(Date, Age) %>% 
        slice(1)
      
      combined_data <- combined_data %>% 
        dplyr::select(-Age) %>% 
        arrange(Date)
      
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
#
#
#
#
####Summary data
#
#Check data ranges for summary data:
date_range_check <- function(file_path){
  ##Load wb and get all sheets
  if(exists("wb")){wb <- wb} else {wb <- loadWorkbook(file_path)}
  sheet_names <- getSheetNames(file_path)
  # Initialize a list to store min and max dates for each sheet
  date_ranges <- list()
  # Loop through each sheet
  for (sheet in sheet_names) {
    # Read the sheet into a data frame
    sheet_data <- read.xlsx(file_path, sheet = sheet)
    # Check if the "Date" column exists
    if ("Date" %in% names(sheet_data)) {
      # Extract the Date column
      date_column <- sheet_data$Date
      # Check if the Date column is of Date type
      if (inherits(date_column, "Date")) {
        # Calculate min and max dates and store in the list
        min_date <- min(date_column, na.rm = TRUE)
        max_date <- max(date_column, na.rm = TRUE)
        date_ranges[[sheet]] <- list(min_date = min_date, max_date = max_date)
      } else {
        #If Cate column is not of Date type, convert, get min and max dates, and store in the list
        date_column <- as.Date(date_column, origin = "1899-12-30")
        min_date <- min(date_column, na.rm = TRUE)
        max_date <- max(date_column, na.rm = TRUE)
        date_ranges[[sheet]] <- list(min_date = min_date, max_date = max_date)
        warning(paste("The 'Date' column in sheet", sheet, "was not loaded as Date type. Column was transformed and may need checked."))
      }
    } else {
      warning(paste("The 'Date' column does not exist in sheet", sheet))
    }
  }
  data_ranges_table <- date_ranges %>% as.data.frame() %>% 
    pivot_longer(cols = everything(), names_to = "Column_Name", values_to = "Value") %>% 
    mutate(Station = sub("\\..*", "", Column_Name), Param = sub(".*\\.", "", Column_Name)) %>% 
    dplyr::select(-Column_Name) %>% pivot_wider(names_from = "Param", values_from = "Value")
  return(data_ranges_table)
  print(data_ranges_table)
}
#
#Create summary data, add to workbook, create sheet as needed. 
data_summary_output <- function(Station_list, Dates, Summary_sheet_name, Summ_type){
  
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
  #
  # Merge all data frames
  merged_data <- Reduce(function(x, y) merge(x, y, by = c("Analysis_Date", "Estuary", "Date"), all = TRUE), sheets_data)
  #
  # Summarize data
  summ_type_clean <- tolower(Summ_type)
  if(summ_type_clean == "sum"){
    Summarized_data <- merged_data %>% 
      mutate(SUMM = rowSums(select(., 4:ncol(.)), na.rm = TRUE)) %>%
      rename(!!Site_sum_name := SUMM)
  } else if (summ_type_clean == "mean"){
    Summarized_data <- merged_data %>% 
      mutate(MEAN = rowMeans(select(., 4:ncol(.)), na.rm = TRUE)) %>%
      rename(!!Site_sum_name := MEAN)
  } else {
    stop(paste0("Error: Unknown Summ_type '", Summ_type, "'. Expected 'Mean' or 'Sum'."))
  }
  
  #
  #Check if wb currently loaded. Remove and reload to make sure correct data.
  options(warn = -1)
  if(exists("wb")){rm(wb)}
  if(exists("existing_data")){rm(existing_data)}
  if(exists("combined_data")){rm(combined_data)}
  options(warn = 0)
  #
  #Output/save summary data - if exists append, if doesn't add new sheet
  if (Site_sum_name %in% excel_sheets(shared_file_path)) {
    #Load workbook
    wb <- loadWorkbook(shared_file_path)
    # Get the existing data from the specified sheet, add new data, keep only newest data if duplicates, and write back to same sheet
    existing_data <- read.xlsx(shared_file_path, sheet = Site_sum_name) %>% 
      mutate(Age = "Older", Analysis_Date = as.Date(Analysis_Date, origin = "1899-12-30"), Date = as.Date(Date, origin = "1899-12-30"))
    combined_data <- rbind(existing_data, Summarized_data %>% mutate(Age = "Newer")) %>% group_by(Date) %>% arrange(Date, Age) %>% slice(1)
    combined_data <- combined_data %>% dplyr::select(-Age) %>% arrange(Date)
    writeData(wb, sheet = Site_sum_name, combined_data)
    # Save the workbook
    saveWorkbook(wb, shared_file_path, overwrite = TRUE)
    print(paste0("Sheet exists for ", Site_sum_name, ". New data was appended to sheet."))
  } else {
    #Load workbook, add worksheet and write data to sheet
    wb <- loadWorkbook(shared_file_path)
    addWorksheet(wb, Site_sum_name)
    writeData(wb, Site_sum_name, Summarized_data)
    saveWorkbook(wb, shared_file_path, overwrite = TRUE)
    print(paste0("Sheet did NOT exist for ", Site_sum_name, " so was created."))
  }
  print("Head of summarized data sheet:")
  print(head(Summarized_data, 10))
}