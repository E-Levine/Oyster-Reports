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
#
#
###Working
#
#
#working_raw_data <- function(Version_type){
#  if(Version_type == "New"){
#    #Read the Excel file
#    t <- readWorkbook(paste0("Local_data/", Estuary_code, "_", Station, "_", ID, ".xlsx"), sheet = 1, detectDates = TRUE)
#    #Determine where to start data based on empty row
#    start_row <- which(t[[1]] == "Station")[1]
#    if(!is.na(start_row)){
#      Raw_data <- t[start_row:nrow(t),]
#    } else {
#      Raw_data <- t
#    }
    #Promote first row to column names, remove columns of NAs, make sure data is only for desired DBKey
#    Raw_data <- Raw_data %>% slice(-1) %>% setNames(unlist(Raw_data[1,])) %>% 
#      dplyr::select(where(~ !all(is.na(.)))) %>% filter(DBKEY == ID) 
    #
#  } else {
#    Raw_data <- c("No data found.")
#  }
#  return(Raw_data)
#}
#