# Filter data frames so that only data collected for inclusion in the Report are present.

### Location Data ###
FixedLocations1 <- dboFixedLocations %>% 
  mutate(StationNumber = as.numeric(StationNumber),
         DateLastCultched = as.Date(DateLastCultched),
         SectionName = case_when(
           SectionName == "W" ~ "West",
           SectionName == "C" ~ "Central",
           SectionName == "E" ~ "East",
           TRUE ~ SectionName)) %>%
  select(FixedLocationID,
         Estuary,
         SectionName,
         StationName,
         ParcelName,
         StationNumber,
         StationNameNumber,
         DateLastCultched,
         ParcelArea) %>% 
  distinct()

    ### Remove intermediate data frames
rm(dboFixedLocations)
###


### Water Quality Data ###
###########
###########
###########
# Space holder for doing that for WQ, drought, and flow data
###########
###########
###########

###


### Survey Data ###
    ### QUADRATS ###  
    # Survey QUADRATS #
Survey_Quad1 <- hsdbSurveyQuadrat %>%
  select(QuadratID,
         NumLive,
         NumDead,
         NumDrills,
         TotalWeight,
         Comments)

    # SBM QUADRATS # 
SBM_Quad1 <- hsdbSBMQuadrat %>%
  rename(TotalWeight = TotalSampleWeight, # Must rename the columns so we can union them together
         NumLive = NumLiveOysters,
         NumDead = NumDeadOysters) %>%
  select(QuadratID,
         NumLive,
         NumDead,
         NumDrills,
         TotalWeight,
         Comments)

    # ALL Quadrats # 
Quadrats_ALL <- union(Survey_Quad1, SBM_Quad1) %>%
  FilterFunction1() %>%
  select(QuadratID,
         FixedLocationID,
         TripDate,
         Estuary,
         SectionName,
         StationName,
         StationNumber,
         ParcelName,
         StationNameNumber,
         ProjectGroup,
         TimeSinceCultched,
         Year,
         NumLive,
         NumDead,
         NumDrills,
         TotalWeight,
         Comments)
    ###

    ### SHELL HEIGHTS ###
    # Survey SHELL HEIGHTS #
Survey_SH1 <- hsdbSurveySH %>%
  select(ShellHeightID, 
         QuadratID, 
         ShellHeight,
         Comments)

    # SBM SHELL HEIGHTS #
SBM_SH1 <- hsdbSBMSH %>%
  filter(LiveOrDead != "Dead") %>% ### Remove measures of dead
  select(ShellHeightID, 
         QuadratID, 
         ShellHeight,
         Comments)

    # ALL Shell Heights #
ShellHeights_ALL <- union(Survey_SH1, SBM_SH1) %>%
  FilterFunction1() %>%
  select(QuadratID,
         FixedLocationID,
         TripDate,
         Estuary,
         SectionName,
         StationName,
         StationNumber,
         ParcelName,
         StationNameNumber,
         ProjectGroup,
         TimeSinceCultched,
         Year,
         ShellHeight,
         Comments)
    ###

    ### Remove intermediate data frames
rm(hsdbSBMQuadrat, hsdbSBMSH, hsdbSurveyQuadrat, hsdbSurveySH)
rm(SBM_Quad1, SBM_SH1, Survey_Quad1, Survey_SH1)
    ###

###


### Recruitment Data ###
# Recruitment
Recruitment <- hsdbRecruitment %>%
  filter(ShellPosition %in% c(2, 3, 4, 5, 8, 9, 10, 11)) %>%
  mutate(DeployedDate = as.Date(DeployedDate), 
         RetDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"), 
         FixedLocationID = substring(ShellID, 19, 22), 
         NumDays = as.numeric(RetDate - DeployedDate),
         BottomMonth = NumBottom/(NumDays / 28),
         AnalysisDate = as.Date(floor_date(RetDate, unit = "month")),
         Plot_Date = as.Date(AnalysisDate + 14)) %>%
  left_join(FixedLocations1, by = c("FixedLocationID")) 
###

### Remove intermediate data frames
rm(hsdbRecruitment)
###


### Condition Index Data ###
    # Condition Index
ConditionIndex <- hsdbConditionIndex %>%
  FilterFunction2() %>%
  select(OysterID,
         SampleEventID,
         FixedLocationID,
         TripDate,
         Estuary,
         SectionName,
         StationName,
         StationNumber,
         StationNameNumber,
         Year,
         ShellHeight, # unnecessary, not in model
         ShellLength, # unnecessary, not in model
         ShellWidth, # unnecessary, not in model
         TotalWeight,
         TissueWetWeight,
         ShellWetWeight,
         TissueDryWeight,
         ShellDryWeight, # unnecessary, not in model
         TarePanWeight,
         Comments)
###

### Remove intermediate data frames
rm(hsdbConditionIndex)
###


### Dermo Data ###
# Dermo
Dermo <- hsdbDermo %>%
  FilterFunction2() %>%
  select(OysterID,
         SampleEventID,
         FixedLocationID,
         TripDate,
         Estuary,
         SectionName,
         StationName,
         StationNumber,
         StationNameNumber,
         Year,
         ShellHeight,
         ShellLength,
         ShellWidth,
         TotalWeight,
         DermoMantle,
         DermoGill,
         Comments)
###

### Remove intermediate data frames
rm(hsdbDermo)
###


### Buceph & Repro Data ###
# Buceph
Buceph <- hsdbRepro %>%
  FilterFunction2() %>%
  select(OysterID,
         SampleEventID,
         FixedLocationID,
         TripDate,
         Estuary,
         SectionName,
         StationName,
         StationNumber,
         StationNameNumber,
         Year,
         Parasite,
         Comments)

# Repro
Repro <- hsdbRepro %>%
  left_join(Dermo, by = 'OysterID') %>%
  select(OysterID,
         SampleEventID.x,
         FixedLocationID,
         TripDate,
         Estuary,
         SectionName,
         StationName,
         StationNumber,
         StationNameNumber,
         Year,
         ShellHeight,
         ShellLength,
         ShellWidth,
         TotalWeight,
         Sex,
         ReproStage,
         Parasite,
         BadSlide,
         Comments.x) %>%
  rename(SampleEventID = SampleEventID.x,
         Comments = Comments.x)
###

### Remove intermediate data frames
rm(hsdbRepro)
###
###