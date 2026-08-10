# Load all data for Report

# Load External Data
###########
###########
###########
# Space holder for doing that for WQ, drought, and flow data
###########
###########
###########
# Temp, Sal, DO data download from DEP cannot be automated. Follow instructions in /data/README.md 
# Identify the TSDO file
file_DEP <- list.files(path = "DMFM/Annual_Report/data/",
                       pattern = "TSDO_*",
                       full.names = TRUE)
# Import the data
TSDO <- read.csv(file_DEP)
TSDO_AccessedDate <- as.Date(substring(file_DEP, 30, 37), # saves the accessed date for future use
                             format = "%Y%m%d")
rm(file_DEP) # remove value once it is not needed

# Load Database Data
# Connect to Local database server and pull all necessary data, then close connection 
con <- dbConnect(odbc(),
                 Driver = "SQL Server", 
                 Server = Server,
                 Database = Database,
                 Authentication = "ActiveDirectoryIntegrated")

# Load Fixed location information
dboFixedLocations <- tbl(con,in_schema("dbo", "FixedLocations")) %>%
  collect() %>% 
  filter(Estuary %in% Estuaries)

# Load Water Quality Data
hsdbWaterQuality <- tbl(con,in_schema("hsdb", "SampleEventWQ")) %>%
  collect() %>%
  filter(substring(SampleEventID, 1, 2) %in% Estuaries)

# Load Survey Data
hsdbSurveyQuadrat <- tbl(con,in_schema("hsdb", "SurveyQuadrat")) %>%
  collect() %>%
  filter(substring(SampleEventID, 1, 2) %in% Estuaries)

hsdbSurveySH <- tbl(con,in_schema("hsdb", "SurveySH")) %>%
  collect() %>%
  mutate(ShellHeight = as.integer(ShellHeight)) %>%
  filter(substring(QuadratID, 1, 2) %in% Estuaries)

hsdbSBMQuadrat <- tbl(con,in_schema("hsdb", "ShellBudgetQuadrat")) %>%
  collect() %>%
  filter(substring(SampleEventID, 1, 2) %in% Estuaries)

hsdbSBMSH <- tbl(con,in_schema("hsdb", "ShellBudgetSH")) %>%
  collect() %>%
  filter(substring(QuadratID, 1, 2) %in% Estuaries)

# Load Condition Index Data
hsdbConditionIndex <- tbl(con,in_schema("hsdb", "ConditionIndex")) %>%
  collect() %>%
  filter(substring(SampleEventID, 1, 2) %in% Estuaries)

# Load Dermo Data
hsdbDermo <- tbl(con,in_schema("hsdb", "Dermo")) %>%
  collect() %>%
  filter(substring(SampleEventID, 1, 2) %in% Estuaries)

# Load Buceph & Repro Data
hsdbRepro <- tbl(con,in_schema("hsdb", "Repro")) %>%
  collect() %>%
  filter(substring(SampleEventID, 1, 2) %in% Estuaries)

# Load Recruitment Data
hsdbRecruitment <- tbl(con,in_schema("hsdb", "Recruitment")) %>%
  collect() %>%
  filter(substring(SampleEventID, 1, 2) %in% Estuaries)

# Disconnect from database
DBI::dbDisconnect(con)
