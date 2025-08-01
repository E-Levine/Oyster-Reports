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
Database <- "Oysters_25-04-25"  #Set the local database to use
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
  filter(as.Date(substr(QuadratID, 8, 15), format = "%Y%m%d") > as.Date(paste0("01/01/", Annual_year), "%m/%d/%Y") & as.Date(substr(QuadratID, 8, 15), format = "%Y%m%d") < as.Date(paste0("12/31/", Annual_year), "%m/%d/%Y")) %>%
  mutate(SH_n = as.integer(substr(ShellHeightID, 29,31))) %>% filter(SH_n < 11)
#
#Cage
hsdbCage <- tbl(con,in_schema("hsdb", "CageCount")) %>%
  collect() %>% mutate(FixedLocationID = substring(SampleEventID, 19, 22)) %>% #Create FixedLocationID column and filter to matching IDs
  filter(FixedLocationID %in% dboFixedLocations$FixedLocationID) %>%
  filter(as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") > as.Date(paste0("01/01/", Annual_year), "%m/%d/%Y") & as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") < as.Date(paste0("12/31/", Annual_year), "%m/%d/%Y"))
hsdbCageSH <- tbl(con,in_schema("hsdb", "CageSH")) %>%
  collect() %>% mutate(FixedLocationID = substring(CageCountID, 19, 22)) %>% #Create FixedLocationID column and filter to matching IDs
  filter(FixedLocationID %in% dboFixedLocations$FixedLocationID) %>%
  filter(as.Date(substr(CageCountID, 8, 15), format = "%Y%m%d") > as.Date(paste0("01/01/", Annual_year), "%m/%d/%Y") & as.Date(substr(CageCountID, 8, 15), format = "%Y%m%d") < as.Date(paste0("12/31/", Annual_year), "%m/%d/%Y")) %>%
  mutate(SH_n = as.integer(substr(ShellHeightID, 30,32)))
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
WQ_df <- hsdbSampleEventWQ %>% left_join(dboFixedLocations %>% dplyr::select(FixedLocationID, Estuary, SectionName, StationNumber)) %>%
  mutate(Project = 'CERP',
         AnalysisDate = as.Date(paste0(substr(SampleEventID, 12, 13), "/01/", substr(SampleEventID, 8, 11)), "%m/%d/%Y"),
         Month = as.numeric(substr(SampleEventID, 12, 13)),
         Year = as.numeric(substr(SampleEventID, 8, 11)),
         Date = as.Date(substr(SampleEventID, 8, 15), "%Y%m%d"),
         Site = paste0(Estuary, "-", SectionName),
         StationNumber = as.numeric(StationNumber),
         Time = paste0(substr(CollectionTime, 1, nchar(CollectionTime) - 2), ":", substr(CollectionTime, nchar(CollectionTime) - 1, nchar(CollectionTime))),
         Chla = -999) %>%
  rename("Station" = StationNumber, "Temp" = Temperature, "DO" = DissolvedOxygen, "DOPct" = PercentDissolvedOxygen) %>%
  dplyr::select(Project:Site, Station, Time, Depth, Temp, Salinity, pH, DO, DOPct, Secchi, Chla) %>% arrange(AnalysisDate, Site, Station)
#
Rcrt_df <- hsdbRcrt %>% left_join(dboFixedLocations %>% dplyr::select(FixedLocationID, Estuary, SectionName, StationNumber)) %>%
  mutate(Project = 'CERP',
         AnalysisDate = as.Date(paste0(substr(SampleEventID, 12, 13), "/01/", substr(SampleEventID, 8, 11)), "%m/%d/%Y"),
         Month = as.numeric(substr(SampleEventID, 12, 13)),
         Year = as.numeric(substr(SampleEventID, 8, 11)),
         RetDate = as.Date(substr(SampleEventID, 8, 15), "%Y%m%d"),
         JulRet = as.numeric(as.Date(substr(SampleEventID, 8, 15), "%Y%m%d")-as.Date("2005-01-01")+1),
         JulDep = as.numeric(as.Date(DeployedDate, "%Y-%m-%d")-as.Date("2005-01-01")+1),
         Site = paste0(Estuary, "-", SectionName),
         NumBottom = as.numeric(ifelse(is.na(NumBottom), -999, NumBottom)),
         NumTop = as.numeric(ifelse(is.na(NumTop), -999, NumTop))) %>%
  mutate(NumDays = as.numeric(RetDate - as.Date(DeployedDate, "%Y-%m-%d"))) %>%
  rename("Station" = StationNumber, "Rep" = ShellReplicate, "Shell" = ShellPosition, "Bottom" = NumBottom, "Top" = NumTop) %>%
  dplyr::select(Project:JulDep, NumDays, Site, Station, Rep, Shell, Bottom, Top) %>% arrange(AnalysisDate, Site, Station, Rep, Shell)
  
#
Dermo_df <- hsdbDermo %>% left_join(dboFixedLocations %>% dplyr::select(FixedLocationID, Estuary, SectionName, StationNumber)) %>%
  mutate(Project = 'CERP',
         AnalysisDate = as.Date(paste0(substr(SampleEventID, 12, 13), "/01/", substr(SampleEventID, 8, 11)), "%m/%d/%Y"),
         Month = as.numeric(substr(SampleEventID, 12, 13)),
         Year = as.numeric(substr(SampleEventID, 8, 11)),
         Date = as.Date(substr(SampleEventID, 8, 15), "%Y%m%d"),
         Site = paste0(Estuary, "-", SectionName),
         Section = -999, 
         SampleNum = as.integer(substr(OysterID, nchar(OysterID)-1, nchar(OysterID))),
         ShellLength = as.numeric(ifelse(is.na(ShellLength), -999, ShellLength)),
         ShellWidth = as.numeric(ifelse(is.na(ShellWidth), -999, ShellWidth))) %>%
  filter(SampleNum < 16) %>%
  rename("SH" = ShellHeight, "SL" = ShellLength, "SW" = ShellWidth, "TotalWt" = TotalWeight, "ShellWetWt" = ShellWetWeight, "Station" = StationNumber) %>%
  dplyr::select(Project:SampleNum, Station, SH:DermoGill) %>% arrange(AnalysisDate, Site, Station, SampleNum)
  
#
Srvy_df_t <- hsdbSrvy %>% dplyr::select(FixedLocationID, QuadratID:TotalWeight) %>% 
  left_join(dboFixedLocations %>% dplyr::select(FixedLocationID, Estuary, SectionName, StationNumber)) %>%
  mutate(Project = 'CERP',
         SurveyDate = as.Date(paste0(substr(SampleEventID, 12, 13), "/01/", substr(SampleEventID, 8, 11)), "%m/%d/%Y"),
         Month = as.numeric(substr(SampleEventID, 12, 13)),
         Year = as.numeric(substr(SampleEventID, 8, 11)),
         StationName = "Z",
         Site = paste0(Estuary, "-", SectionName),
         Date = as.Date(substr(SampleEventID, 8, 15), "%Y%m%d"),
         Section = "Z", 
         Live = -999, Dead = -999, TotalVolume = -999, TotalWeight = -999) %>%
  mutate(Season = case_when(month(SurveyDate) == 3 ~ 'Spr', 
                            month(SurveyDate) == 6 ~ 'Sum', 
                            month(SurveyDate) == 9 ~ 'Fal', 
                            month(SurveyDate) == 12 ~ 'Win', TRUE ~ 'UNK'),
         Survey = paste0(case_when(month(SurveyDate) == 3 ~ 'Spring', 
                                   month(SurveyDate) == 6 ~ 'Summer', 
                                   month(SurveyDate) == 9 ~ 'Fall', 
                                   month(SurveyDate) == 12 ~ 'Winter', TRUE ~ 'UNK'), year(SurveyDate) %% 100)) %>%
  rename("Station" = StationNumber, "Quadrat" = QuadratNumber, "LiveQtr" = NumLive, "DeadQtr" = NumDead, "Volume" = TotalVolume, "Weight" = TotalWeight) %>%
  dplyr::select(Project:StationName, Season, Survey, Site:Section, Station, Quadrat:DeadQtr, Live, Dead, Volume, Weight)
Srvy_df <- Srvy_df_t %>% anti_join(Srvy_df_t %>% filter((Season == 'Sum' & Site == "CR-W" & Station == 4) | (Season == 'Sum' & Site == "CR-E" & Station == 1) | (Season == 'Win' & Site == "CR-E" & Station == 1) | (Season == 'Win' & Site == "CR-W" & Station == 4))) %>%
  arrange(SurveyDate, Site, Station, Quadrat)
#
SrvySH_df_t <- hsdbSrvySH %>% left_join(dboFixedLocations %>% dplyr::select(FixedLocationID, Estuary, SectionName, StationNumber)) %>%
  mutate(Project = 'CERP',
         SurveyDate = as.Date(paste0(substr(QuadratID, 12, 13), "/01/", substr(QuadratID, 8, 11)), "%m/%d/%Y"),
         Month = as.numeric(substr(QuadratID, 12, 13)),
         Year = as.numeric(substr(QuadratID, 8, 11)),
         StationName = -999,
         Site = paste0(Estuary, "-", SectionName),
         Date = as.Date(substr(QuadratID, 8, 15), "%Y%m%d"),
         Quadrat = as.integer(substr(QuadratID, nchar(QuadratID)-1, nchar(QuadratID)))) %>%
  mutate(Season = case_when(month(SurveyDate) == 3 ~ 'Spr', 
                            month(SurveyDate) == 6 ~ 'Sum', 
                            month(SurveyDate) == 9 ~ 'Fal', 
                            month(SurveyDate) == 12 ~ 'Win', TRUE ~ 'UNK'),
         Survey = paste0(case_when(month(SurveyDate) == 3 ~ 'Spring', 
                                   month(SurveyDate) == 6 ~ 'Summer', 
                                   month(SurveyDate) == 9 ~ 'Fall', 
                                   month(SurveyDate) == 12 ~ 'Winter', TRUE ~ 'UNK'), year(SurveyDate) %% 100),
         ShellHeight = as.numeric(ifelse(is.na(ShellHeight), -999, ShellHeight))) %>%
  rename("Station" = StationNumber, "SH" = ShellHeight) %>% 
  dplyr::select(Project:StationName, Season, Survey, Site, Date, Station, Quadrat, SH) %>% arrange(SurveyDate, Site, Station, Quadrat)
#
SrvySH_df <- SrvySH_df_t %>% anti_join(SrvySH_df_t %>% filter((Season == 'Sum' & Site == "CR-W" & Station == 4) | (Season == 'Sum' & Site == "CR-E" & Station == 1) | (Season == 'Win' & Site == "CR-E" & Station == 1) | (Season == 'Win' & Site == "CR-W" & Station == 4))) %>%
  arrange(SurveyDate, Site, Station, Quadrat)
#
Cage_df <- hsdbCage %>% dplyr::select(CageCountID, DeployedDate, RetrievedDate, DaysDeployed, CageColor, FixedLocationID) %>% 
  left_join(dboFixedLocations %>% dplyr::select(FixedLocationID, Estuary, SectionName, StationNumber)) %>% 
  mutate(CageCountID = paste0(substring(CageCountID, 1, 22), "_", substring(CageCountID, 28, 28))) %>%
  mutate(AnalysisDateRet = as.Date(paste0(substr(CageCountID, 12, 13), "/01/", substr(CageCountID, 8, 11)), "%m/%d/%Y"),
         Month = as.numeric(substr(CageCountID, 12, 13)),
         Year = as.numeric(substr(CageCountID, 8, 11)),
         RetJulian = as.numeric(as.Date(substr(CageCountID, 8, 15), "%Y%m%d")-as.Date("2005-01-01")+1),
         DepJulian = as.numeric(as.Date(DeployedDate, "%Y-%m-%d")-as.Date("2005-01-01")+1),
         Site = paste0(Estuary, "-", SectionName),
         SiteSta = paste(Site, StationNumber, sep = " ")) %>%
  full_join(hsdbCageSH %>% dplyr::select(ShellHeightID, CageCountID, ShellHeight, SH_n) %>% mutate(Type = case_when(substring(CageCountID, 26, 26) == "D" ~ "DepSH", TRUE ~ "RetSH")) %>%  mutate(CageCountID = paste0(substring(CageCountID, 1, 22), "_", substring(CageCountID, 28, 28)), ShellHeightID = paste0(substring(ShellHeightID, 1, 22),"_",substring(ShellHeightID, 28, 31))) %>% pivot_wider(values_from = ShellHeight, names_from = Type) %>% arrange(SH_n)) %>%
  dplyr::select(AnalysisDateRet, Month, Year, "DepDate" = DeployedDate, "RetDate" = RetrievedDate, DepJulian, RetJulian, "Days" = DaysDeployed, Site, SiteSta, "Station"  = StationNumber, "Cage" = CageColor, DepSH, RetSH, ShellHeightID) %>%
  arrange(AnalysisDateRet, SiteSta, Cage) %>% mutate_at(c("DepSH", "RetSH"), as.numeric) %>% mutate(DepDate = as.Date(DepDate, format = "%Y-%m-%d"), RetDate = as.Date(RetDate, format = "%Y-%m-%d")) %>%
  distinct()
#
Flow_df <- dsFlow %>% 
  mutate(Year = as.numeric(format(Date, "%Y")),
         Month = as.numeric(format(Date, "%m"))) %>%
  rename("AnalysisDate" = Analysis_Date) %>%
  dplyr::select(AnalysisDate, Year, Month, Site, Date, FlowSum)
#
#
##
#
####Save all modified data to output####
#
#Load each sheet and append new data
WQ_excel <- readWorkbook("CERP/CERP_PBC_Raw_Data.xlsx", sheet = 'WaterQuality', detectDates = TRUE) %>% mutate(across(contains("Date"), ~as.Date(.x, format = "%Y-%m-%d"))) %>% 
 # mutate(Time = format((as.POSIXct(Time*86400, origin = "1970-01-01", tz = "UTC")), "%H:%M")) %>% 
  bind_rows(WQ_df)
Flow_excel <- readWorkbook("CERP/CERP_PBC_Raw_Data.xlsx", sheet = 'Flow', detectDates = TRUE) %>% mutate(across(contains("Date"), ~as.Date(.x, format = "%Y-%m-%d"))) %>% bind_rows(Flow_df)
Rcrt_excel <- readWorkbook("CERP/CERP_PBC_Raw_Data.xlsx", sheet = 'Rcrt', detectDates = TRUE) %>% mutate(across(contains("Date"), ~as.Date(.x, format = "%Y-%m-%d"))) %>% bind_rows(Rcrt_df)
Dermo_excel <- readWorkbook("CERP/CERP_PBC_Raw_Data.xlsx", sheet = 'Dermo', detectDates = TRUE) %>% mutate(across(contains("Date"), ~as.Date(.x, format = "%Y-%m-%d"))) %>% bind_rows(Dermo_df)
Survey_excel <- readWorkbook("CERP/CERP_PBC_Raw_Data.xlsx", sheet = 'SurveyCounts', detectDates = TRUE) %>% mutate(across(contains("Date"))) %>% bind_rows(Srvy_df)
SH_excel <- readWorkbook("CERP/CERP_PBC_Raw_Data.xlsx", sheet = 'SurveySHs', detectDates = TRUE) %>% mutate(across(contains("Date"), ~as.Date(.x, format = "%Y-%m-%d"))) %>% bind_rows(SrvySH_df)
Growth_excel <- readWorkbook("CERP/CERP_PBC_Raw_Data.xlsx", sheet = 'Growth', detectDates = TRUE) %>% mutate(across(contains("Date"), ~as.Date(.x, format = "%Y-%m-%d"))) %>% bind_rows(Cage_df)

#Load workbook of existing data
Data_wb <- loadWorkbook("CERP/CERP_PBC_Raw_Data.xlsx")
#Overwrite each sheet with new data appended to old
writeData(Data_wb, "WaterQuality", WQ_excel)
writeData(Data_wb, "Flow", Flow_excel)
writeData(Data_wb, "Rcrt", Rcrt_excel)
writeData(Data_wb, "Dermo", Dermo_excel)
writeData(Data_wb, "SurveyCounts", Survey_excel)
writeData(Data_wb, "SurveySHs", SH_excel)
writeData(Data_wb, "Growth", Growth_excel)

#Save workbook
saveWorkbook(Data_wb, "CERP/CERP_PBC_Raw_Data.xlsx", overwrite=TRUE)
