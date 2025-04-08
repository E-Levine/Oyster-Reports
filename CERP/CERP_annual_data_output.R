##CERP annual data
#
#Extraction of annual data for updating SAS data files - only fixed locations
#
#
#
#
####Packages and setup####
#
# Load necessary R packages
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(odbc, DBI, dbplyr,
               tidyverse, dplyr,  #DF manipulation
               #readxl,          #Excel
               lubridate,         #Dates
               knitr, here,
               flextable, openxlsx,
               install = TRUE)
#
#
Database <- "Oysters_25-03-31"  #Set the local database to use
Server = "localhost\\ERICALOCALSQL" #Set the local Server to use
Estuary_codes <- c("CR", "SL", "LX")
Annual_year <- c("2024")
#
#
##
#
####Data downloads - run without changes####
#
## Connect to Local database server and pull all necessary data, then close connection 
con <- dbConnect(odbc(),
                 Driver = "SQL Server", 
                 Server = Server,
                 Database = Database,
                 Authentication = "ActiveDirectoryIntegrated")
#
dboFixedLocations <- tbl(con,in_schema("dbo", "FixedLocations")) %>%
  collect() %>% filter(Estuary %in% Estuary_codes & grepl("^0", FixedLocationID)) %>% #Limit to primary stations, remove "ARRA" and SLN4
  filter(!grepl("^ARRA", StationName) & StationName != "SL-N-4")
#
#Water quality
hsdbSampleEventWQ <- tbl(con,in_schema("hsdb", "SampleEventWQ")) %>%
  collect() %>% mutate(FixedLocationID = substring(SampleEventID, 19, 22)) %>% #Create FixedLocationID column and filter to matching IDs
  filter(FixedLocationID %in% dboFixedLocations$FixedLocationID & str_detect(SampleEventWQID, 'COLL')) %>% #Limit to COLL (dermo) data (Pre-2023, WQ is with RCRT. 2023+ is with COLL)
  filter(as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") > as.Date(paste0("01/01/", Annual_year), "%m/%d/%Y") & as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") < as.Date(paste0("12/31/", Annual_year), "%m/%d/%Y"))
#
#Dermo
hsdbDermo <- tbl(con,in_schema("hsdb", "Dermo")) %>%
  collect() %>% mutate(FixedLocationID = substring(SampleEventID, 19, 22)) %>% #Create FixedLocationID column and filter to matching IDs
  filter(FixedLocationID %in% dboFixedLocations$FixedLocationID) %>%
  filter(as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") > as.Date(paste0("01/01/", Annual_year), "%m/%d/%Y") & as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") < as.Date(paste0("12/31/", Annual_year), "%m/%d/%Y"))
#
#Recruitment
hsdbRcrt <- tbl(con,in_schema("hsdb", "Recruitment")) %>%
  collect() %>% mutate(FixedLocationID = substring(SampleEventID, 19, 22)) %>% #Create FixedLocationID column and filter to matching IDs
  filter(FixedLocationID %in% dboFixedLocations$FixedLocationID) %>%
  filter(as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") > as.Date(paste0("01/01/", Annual_year), "%m/%d/%Y") & as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") < as.Date(paste0("12/31/", Annual_year), "%m/%d/%Y"))
#
#Survey
hsdbSrvy <- tbl(con,in_schema("hsdb", "SurveyQuadrat")) %>%
  collect() %>% mutate(FixedLocationID = substring(SampleEventID, 19, 22)) %>% #Create FixedLocationID column and filter to matching IDs
  filter(FixedLocationID %in% dboFixedLocations$FixedLocationID) %>%
  filter(as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") > as.Date(paste0("01/01/", Annual_year), "%m/%d/%Y") & as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") < as.Date(paste0("12/31/", Annual_year), "%m/%d/%Y"))
hsdbSrvySH <- tbl(con,in_schema("hsdb", "SurveySH")) %>%
  collect() %>% mutate(FixedLocationID = substring(QuadratID, 19, 22)) %>% #Create FixedLocationID column and filter to matching IDs
  filter(FixedLocationID %in% dboFixedLocations$FixedLocationID) %>%
  filter(as.Date(substr(QuadratID, 8, 15), format = "%Y%m%d") > as.Date(paste0("01/01/", Annual_year), "%m/%d/%Y") & as.Date(substr(QuadratID, 8, 15), format = "%Y%m%d") < as.Date(paste0("12/31/", Annual_year), "%m/%d/%Y"))
  mutate(SH_n = as.integer(substr(ShellHeightID, 29,31))) %>% filter(SH_n < 11)
#
DBI::dbDisconnect(con)
#
#
###Flow data from Excel
dsFlow <- rbind(readWorkbook("DBHYDRO/Shared_data/SL_Flow.xlsx", sheet = 'SLESum', detectDates = TRUE, check.names = TRUE) %>% filter(Date >= as.Date(paste0("01/01/", Annual_year), "%m/%d/%Y") & Date <= as.Date(paste0("12/31/", Annual_year), "%m/%d/%Y")) %>% 
  dplyr::select(Analysis_Date, Date, SLESum) %>% mutate(Site = "SLE") %>% rename("FlowSum" = SLESum),
  readWorkbook("DBHYDRO/Shared_data/LX_Flow.xlsx", sheet = 'LOXSum', detectDates = TRUE, check.names = TRUE) %>% filter(Date >= as.Date(paste0("01/01/", Annual_year), "%m/%d/%Y") & Date <= as.Date(paste0("12/31/", Annual_year), "%m/%d/%Y")) %>% 
    dplyr::select(Analysis_Date, Date, LOXSum) %>% mutate(Site = "LOX") %>% rename("FlowSum" = LOXSum)) %>%
  rbind(readWorkbook("DBHYDRO/Shared_data/CR_Flow.xlsx", sheet = 'S79', detectDates = TRUE, check.names = TRUE) %>% filter(Date >= as.Date(paste0("01/01/", Annual_year), "%m/%d/%Y") & Date <= as.Date(paste0("12/31/", Annual_year), "%m/%d/%Y")) %>% 
          dplyr::select(Analysis_Date, Date, Flow) %>% mutate(Site = "CRE") %>% rename("FlowSum" = Flow))
#
#
##
#
####Modify columns as needed for output####
#
Srvy_df <- hsdbSrvy %>% dplyr::select(FixedLocationID, QuadratID:TotalWeight) %>% 
  left_join(dboFixedLocations %>% dplyr::select(FixedLocationID, Estuary, SectionName, StationNumber)) %>%
  mutate(Project = 'CERP',
         SurveyDate = paste0(substr(SampleEventID, 12, 13), "/01/", substr(SampleEventID, 8, 11)),
         Month = substr(SampleEventID, 12, 13),
         Year = substr(SampleEventID, 8, 11),
         StationName = "Z",
         Site = paste0(Estuary, "-", SectionName),
         Date = as.Date(substr(SampleEventID, 8, 15), "%Y%m%d"),
         Section = "Z", 
         Live = -999, Dead = -999, TotalVolume = -999, TotalWeight = -999) %>%
  mutate(Season = case_when(grepl("^03", SurveyDate) ~ 'Spr', 
                            grepl("^06", SurveyDate) ~ 'Sum', 
                            grepl("^09", SurveyDate) ~ 'Fal', 
                            grepl("^12", SurveyDate) ~ 'Win', TRUE ~ 'UNK'),
         Survey = paste0(case_when(grepl("^03", SurveyDate) ~ 'Spring', 
                                   grepl("^06", SurveyDate) ~ 'Summer', 
                                   grepl("^09", SurveyDate) ~ 'Fall', 
                                   grepl("^12", SurveyDate) ~ 'Winter', TRUE ~ 'UNK'), substr(SurveyDate, 9, 10))) %>%
  rename("Station" = StationNumber, "Quadrat" = QuadratNumber, "LiveQ" = NumLive, "DeadQ" = NumDead, "Volume" = TotalVolume, "Weight" = TotalWeight) %>%
  dplyr::select(Project:StationName, Season, Survey, Site:Section, Station, Quadrat:DeadQ, Live, Dead, Volume, Weight) %>% arrange(SurveyDate, Site, Station, Quadrat)
#
SrvySH_df <- hsdbSrvySH %>% left_join(dboFixedLocations %>% dplyr::select(FixedLocationID, Estuary, SectionName, StationNumber)) %>%
  mutate(Project = 'CERP',
         SurveyDate = paste0(substr(QuadratID, 12, 13), "/01/", substr(QuadratID, 8, 11)),
         Month = substr(QuadratID, 12, 13),
         Year = substr(QuadratID, 8, 11),
         StationName = -999,
         Site = paste0(Estuary, "-", SectionName),
         Date = as.Date(substr(QuadratID, 8, 15), "%Y%m%d"),
         Quadrat = substr(QuadratID, nchar(QuadratID)-1, nchar(QuadratID))) %>%
  mutate(Season = case_when(grepl("^03", SurveyDate) ~ 'Spr', 
                            grepl("^06", SurveyDate) ~ 'Sum', 
                            grepl("^09", SurveyDate) ~ 'Fal', 
                            grepl("^12", SurveyDate) ~ 'Win', TRUE ~ 'UNK'),
         Survey = paste0(case_when(grepl("^03", SurveyDate) ~ 'Spring', 
                                   grepl("^06", SurveyDate) ~ 'Summer', 
                                   grepl("^09", SurveyDate) ~ 'Fall', 
                                   grepl("^12", SurveyDate) ~ 'Winter', TRUE ~ 'UNK'), substr(SurveyDate, 9, 10))) %>%
  rename("Station" = StationNumber, "SH" = ShellHeight) %>% 
  dplyr::select(Project:StationName, Season, Survey, Site, Date, Station, Quadrat, SH) %>% arrange(SurveyDate, Site, Station, Quadrat)
#
Flow_df <- dsFlow %>% 
  mutate(Year = format(Date, "%Y"),
         Month = format(Date, "%m")) %>%
  dplyr::select(Analysis_Date, Year, Month, Site, Date, FlowSum)
#
#
##