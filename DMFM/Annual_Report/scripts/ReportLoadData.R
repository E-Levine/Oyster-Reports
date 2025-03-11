# Load all data for Report

# Load External Data
###########
###########
###########
# Space holder for doing that for WQ, drought, and flow data
###########
###########
###########

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

# Load Dermo Data

# Load Repro Data

# Load Recruitment Data

# Disconnect from database
DBI::dbDisconnect(con)
