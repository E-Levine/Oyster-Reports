# Filter data frames so that only data collected for inclusion in the Report are present.

### Location Data ###
FixedLocations1 <- dboFixedLocations %>% 
  mutate(StationNumber = as.numeric(StationNumber),
         DateLastCultched = as.Date(DateLastCultched)) %>%
  select(FixedLocationID,
         Estuary,
         SectionName,
         StationName,
         ParcelName,
         StationNumber,
         StationNameNumber,
         DateLastCultched) %>% 
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


### Condition Index Data ###
###########
###########
###########
# Space holder for doing that for CI data
###########
###########
###########

###