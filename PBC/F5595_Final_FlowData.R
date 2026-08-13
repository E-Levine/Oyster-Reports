##F5595 Final - Flow data relationships
#
#
pacman::p_load(odbc, DBI, dbplyr,
               tidyverse, dplyr,  stringr, #DF manipulation
               DT, openxlsx, readxl,        #Excel
               lubridate, zoo,         #Dates
               slider, #rolling flow
               minpack.lm, quantreg, #curve fit
               sf, shadowtext, extrafont, #mapping
               glmmTMB, purrr, emmeans, car, broom, multcomp, #Analyses
               knitr, kableExtra, scales, gt, gtExtras,
               scales,
               install = TRUE)
#
#
#### Set up ----
#
Flow_date <- as.Date("2023-09-01") # Date to start flow data (includes 13 days)
Start_date <- as.Date("2023-10-01") # Report start date. Start 13 days prior for rolling sum, then limit data when cleaning
End_date <- as.Date("2026-09-30") # Report end date
Database <- "Oysters_26-08-11"  #Set the local database to use
Server <- "localhost\\ERICALOCALSQL" #Set the local Server to use
Estuaries <- c("LW")
#
loadfonts(device = "win")
runANDsavePlots <- c("N")
#
#
#### Data files ----
#
## Stations
Stations <- readWorkbook("PBC/Data/F5595_Stations.xlsx", sheet = "Sheet1", detectDates = TRUE, check.names = TRUE)
#
## DBHYDRO
Flow_raw <- readWorkbook("DBHYDRO/Shared_data/LW_Flow.xlsx", sheet = paste0(Estuaries, 'Sum'), detectDates = TRUE, check.names = TRUE) %>%
  filter(Date >= Flow_date & Date <= End_date)
# Clean flow, add 7-day rolling sum & 14-day
Flow_df <- Flow_raw %>% 
  #dplyr::select(LWSum) %>%
  #pivot_longer(cols = c(S155_Flow, S41_Flow, S44_Flow), names_to = c("Station", ".value"), names_pattern = "(.*)_(.*)") %>% group_by(Station) %>%
  mutate(Sum7 = slide_index_dbl(.x = LWSum, .i = Date, .f = sum, 
                                .before = days(6)), # Current day + 6 days prior = 7 days total
         Sum14 = slide_index_dbl(.x = LWSum, .i = Date, .f = sum, 
                                 .before = days(13)), # Current day + 13 days prior = 14 days total
         Sum21 = slide_index_dbl(.x = LWSum, .i = Date, .f = sum, 
                                .before = days(20)),
         Sum28 = slide_index_dbl(.x = LWSum, .i = Date, .f = sum, 
                                .before = days(28)),
         Sum7_155 = slide_index_dbl(.x = S155_Flow, .i = Date, .f = sum, 
                                .before = days(6)), # Current day + 6 days prior = 7 days total
         Sum14_155 = slide_index_dbl(.x = S155_Flow, .i = Date, .f = sum, 
                                 .before = days(13)), # Current day + 13 days prior = 14 days total
         Sum21_155 = slide_index_dbl(.x = S155_Flow, .i = Date, .f = sum, 
                                 .before = days(20)),
         Sum28_155 = slide_index_dbl(.x = S155_Flow, .i = Date, .f = sum, 
                                 .before = days(28))
  ) %>%
  #ungroup() %>%
  dplyr::filter(Date >= Start_date & Date <= End_date)
#
#Rain_raw <- readWorkbook("DBHYDRO/Shared_data/LW_Rain.xlsx", sheet = paste0(Estuaries, 'RainSum'), detectDates = TRUE, check.names = TRUE) %>%
#  filter(Date >= Start_date & Date <= End_date)
#
Sali_raw <- readWorkbook("DBHYDRO/Shared_data/LW_Sali.xlsx", sheet = paste0(Estuaries, 'Mean'), detectDates = TRUE, check.names = TRUE) %>%
  filter(Date >= Start_date & Date <= End_date) %>%
  mutate(across(c(LWL20_Sali, LWL19_Sali), ~ ifelse(.x < 0, NA, .x))) %>%
  rowwise() %>%
  mutate(LWMean = mean(c(LWL20_Sali, LWL19_Sali), na.rm = TRUE)) %>%
  ungroup()
#
Turb_raw <- readWorkbook("DBHYDRO/Shared_data/LW_Turb.xlsx", sheet = paste0(Estuaries, 'Mean'), detectDates = TRUE, check.names = TRUE) %>%
  filter(Date >= Start_date & Date <= End_date)
#
#
## Oysters
LocationID_order <- c("0235", "0236", "0237", "0240", "0241", "0312")
# Connect to Local database server and pull all necessary data, then close connection 
con <- dbConnect(odbc(),
                 Driver = "SQL Server", 
                 Server = Server,
                 Database = Database,
                 Authentication = "ActiveDirectoryIntegrated")

dboFixedLocations <- tbl(con,in_schema("dbo", "FixedLocations")) %>%
  collect() %>% 
  filter(Estuary %in% Estuaries & grepl("^0", FixedLocationID)) %>% 
  filter(EndDate >= Start_date)
# Water quality
hsdbSampleEventWQ <- tbl(con,in_schema("hsdb", "SampleEventWQ")) %>%
  collect() %>% 
  #Create FixedLocationID column and filter to matching IDs
  mutate(FixedLocationID = substring(SampleEventID, 19, 22)) %>% 
  filter(FixedLocationID %in% dboFixedLocations$FixedLocationID & str_detect(SampleEventWQID, 'COLL')) %>% 
  filter(as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") > Start_date & as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") < End_date)
dboSampleEventWQ <- tbl(con,in_schema("dbo", "SampleEventWQ")) %>%
  collect() %>% 
  #Create FixedLocationID column and filter to matching IDs
  mutate(FixedLocationID = substring(SampleEventID, 19, 22)) %>% 
  filter(FixedLocationID %in% dboFixedLocations$FixedLocationID & str_detect(SampleEventWQID, 'COLL')) %>% 
  filter(as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") > Start_date & as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") < End_date)
WQ_raw <- left_join(
  rbind((hsdbSampleEventWQ %>% filter(DataStatus == "Proofed" | DataStatus == "Completed")), 
        (dboSampleEventWQ %>% filter(DataStatus == "Proofed"))) %>%
    #Create date columns and Secchi penetration
    mutate(RetDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"),
           AnalysisDate = as.Date(floor_date(RetDate, unit = "month"))+14) %>%
    #Filter data to desired columns needed for summary
    dplyr::select(AnalysisDate, RetDate, SampleEventWQID, SampleEventID, FixedLocationID, Salinity, CollectionTime, SampleDepth, TurbidityYSI, Comments) %>% 
    rename(Sal = Salinity) %>%
    #Arrange by station then date
    arrange(match(FixedLocationID, c(LocationID_order)), AnalysisDate),
  #Add simplified StationName 
  dboFixedLocations %>% 
    mutate(StationName = paste0(Estuary, "-", SectionName, StationNumber),
           Site = paste0(Estuary, "-", SectionName)) %>%
    dplyr::select(FixedLocationID, Site, StationName, StationNumber)) %>%
  filter(!if_all(c("Sal"), ~is.na(.)))
#
# Sediment
Sedi_Sites <- c("LW-L", "LW-R")
Sedi_order <- c("LW-L1", "LW-L2", "LW-L3", "LW-R2", "LW-R3", "LW-R4")
Positions <- c("North", "South")
NorthStations <- c("0312", "0235", "0236")
SouthStations <- c("0240", "0241", "0237")
#
hsdbSedi <- tbl(con,in_schema("hsdb", "SedimentTrap")) %>%
  collect() %>% 
  #Create FixedLocationID column and filter to matching IDs
  mutate(FixedLocationID = substring(SampleEventID, 19, 22)) %>% 
  filter(FixedLocationID %in% dboFixedLocations$FixedLocationID) %>%
  filter(as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") > Start_date & as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") < End_date)
dboSedi <- tbl(con,in_schema("dbo", "SedimentTrap")) %>%
  collect() %>% 
  #Create FixedLocationID column and filter to matching IDs
  mutate(FixedLocationID = substring(SampleEventID, 19, 22)) %>% 
  filter(FixedLocationID %in% dboFixedLocations$FixedLocationID) %>%
  filter(as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") > Start_date & as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d") < End_date)
#
Sedi_raw <- left_join(
  rbind((hsdbSedi %>% filter(DataStatus == "Proofed" | DataStatus == "Completed") %>%
           #Sept 2025 data
           mutate(FilterDryWeight = case_when(grepl("^LWR2509-02", CupSampleID) ~ 3.49, grepl("^LWR2509-03", CupSampleID) ~ 3.40, grepl("^LWR2509-04", CupSampleID) ~ 3.33, TRUE ~ FilterDryWeight), FilterTareWeight = case_when(grepl("^LWR2509-02", CupSampleID) ~ 0, grepl("^LWR2509-03", CupSampleID) ~ 0, grepl("^LWR2509-04", CupSampleID) ~ 0, TRUE ~ FilterTareWeight))), 
        (dboSedi %>% filter(DataStatus == "Proofed"))) %>%
    #Create date columns 
    mutate(RetDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"),
           DeployedDate = as.Date(DeployedDate, format = "%Y-%m-%d"),
           AnalysisDate = as.Date(floor_date(RetDate, unit = "month"))+14,
           Position = case_when(FixedLocationID %in% NorthStations ~ "North",
                                FixedLocationID %in% SouthStations ~ "South",
                                TRUE ~ NA),
           TotalDW = (PanDryWeight - PanTareWeight) + (FilterDryWeight - FilterTareWeight),
           Proportion = round((CrucibleDW-TareCrucible)/((PanDryWeight - PanTareWeight) + (FilterDryWeight - FilterTareWeight)),3),
           TotalAsh = case_when(is.na(Proportion) ~(AshWeight - TareCrucible)*(1/PortionofSample), TRUE ~ (AshWeight - TareCrucible)*(1/Proportion)),
           PctOrganic = ((TotalDW-round(TotalAsh,3))/TotalDW)*100,
           OrganicWt = TotalDW*(PctOrganic/100)) %>%
    #Filter data to desired columns needed for summary
    dplyr::select(AnalysisDate, RetDate, DeployedDate, CupSampleID, SampleEventID, FixedLocationID, Position, TotalDW, PctOrganic, OrganicWt) %>% 
    #Arrange by station then date
    arrange(match(FixedLocationID, c(LocationID_order)), AnalysisDate),
  #Add simplified StationName 
  dboFixedLocations %>% 
    mutate(StationName = paste0(Estuary, "-", SectionName, StationNumber),
           Site = paste0(Estuary, "-", SectionName)) %>%
    dplyr::select(FixedLocationID, Site, StationName, StationNumber)) %>% 
  filter(Site %in% Sedi_Sites)
#
DBI::dbDisconnect(con)
#
rm(hsdbSampleEventWQ, hsdbSedi, dboFixedLocations, dboSampleEventWQ, dboSedi, con)
#
#
#
#
#### Clean and combine ----
#
Salinity <- bind_rows(Sali_raw %>% 
                        dplyr::select(-LWMean) %>%
                        pivot_longer(cols = c(LWL20_Sali, LWL19_Sali),
                                     names_to = c("Station", ".value"),
                                     names_pattern = "(.*)_(.*)") %>%
                        drop_na(Sali) %>%
                        rename(Sal = Sali, AnalysisDate = Analysis_Date),
                      WQ_raw %>% 
                        dplyr::select(AnalysisDate, RetDate, StationName, Sal) %>%
                        mutate(Estuary = "LW") %>%
                        rename(Date = RetDate, Station = StationName))
MeanSalinity <- merge(Salinity %>% 
        group_by(AnalysisDate, Estuary, Station) %>%
        summarise(MeanSal = mean(Sal, na.rm = T)) %>%
        pivot_wider(names_from = "Station", values_from = "MeanSal"),
      Salinity %>% 
        left_join(Stations %>% dplyr::select(StationNam, Section), by = c("Station" = "StationNam")) %>%
        group_by(AnalysisDate, Estuary, Section) %>%
        summarise(MeanSal = mean(Sal, na.rm = T)) %>%
        pivot_wider(names_from = "Section", values_from = "MeanSal"))
head(MeanSalinity)
#
Turbidity <- bind_rows(Turb_raw %>% 
                        dplyr::select(-LWMean) %>%
                        pivot_longer(cols = c(LWL20_Turb, LWL19_Turb),
                                     names_to = c("Station", ".value"),
                                     names_pattern = "(.*)_(.*)") %>%
                        drop_na(Turb) %>%
                        rename(AnalysisDate = Analysis_Date),
                      WQ_raw %>% 
                        dplyr::select(AnalysisDate, RetDate, StationName, TurbidityYSI) %>%
                        mutate(Estuary = "LW") %>%
                        rename(Date = RetDate, Station = StationName, Turb = TurbidityYSI))
MeanTurbidity <- merge(Turbidity %>% 
                        group_by(AnalysisDate, Estuary, Station) %>%
                        summarise(MeanTurb = mean(Turb, na.rm = T)) %>%
                        pivot_wider(names_from = "Station", values_from = "MeanTurb"),
                       Turbidity %>% 
                        left_join(Stations %>% dplyr::select(StationNam, Section), by = c("Station" = "StationNam")) %>%
                        group_by(AnalysisDate, Estuary, Section) %>%
                        summarise(MeanTurb = mean(Turb, na.rm = T)) %>%
                        pivot_wider(names_from = "Section", values_from = "MeanTurb"))
head(MeanTurbidity)
#
#
#
## Sediment data
Sediment_sta <- Sedi_raw %>%
    group_by(AnalysisDate, RetDate, StationName) %>%
    summarise(RateMean = mean(TotalDW/(as.integer(RetDate-DeployedDate)/28), na.rm = T),
              RateSD = sd(TotalDW/(as.integer(RetDate-DeployedDate)/28), na.rm = T),
              PctOrganicMean = mean(PctOrganic, na.rm = T),
              PctOrganicSD = sd(PctOrganic, na.rm = T),
              OrgWtMean = mean(OrganicWt/(as.integer(RetDate-DeployedDate)/28), na.rm = T),
              OrgWtSD = sd(OrganicWt/(as.integer(RetDate-DeployedDate)/28), na.rm = T))
head(Sediment_sta)
#
Sediment_sec <- Sedi_raw %>%
    left_join(Stations %>% dplyr::select(StationNam, Section), by = c("StationName" = "StationNam")) %>%
    group_by(AnalysisDate, RetDate, Section) %>%
    summarise(RateMean = mean(TotalDW/(as.integer(RetDate-DeployedDate)/28), na.rm = T),
              RateSD = sd(TotalDW/(as.integer(RetDate-DeployedDate)/28), na.rm = T),
              PctOrganicMean = mean(PctOrganic, na.rm = T),
              PctOrganicSD = sd(PctOrganic, na.rm = T),
              OrgWtMean = mean(OrganicWt/(as.integer(RetDate-DeployedDate)/28), na.rm = T),
              OrgWtSD = sd(OrganicWt/(as.integer(RetDate-DeployedDate)/28), na.rm = T))
#
head(Sediment_sec)
#
#
#
#
#### Station map ----
#
FL_outline <- st_read("PBC/Data/FL_Outlines/FL_Outlines.shp")
#
Stations_sf <- st_as_sf(Stations, 
                  coords = c("Long", "Lat"), 
                  crs = 4326) # Start with WGS 84
# Map with or without station lavbels
ggplot()+
  geom_sf(data = FL_outline)+
  #Individual station points if grouping:
  geom_sf(data = Stations_sf, aes(color = Source, shape = Source), alpha = 0.8, size = 4)+
  #Station labels - DBHydro
  geom_shadowtext(data = Stations %>% filter(Source != "Oysters"), 
                  aes(Long, Lat, label = StationNam),
                  nudge_x = -0.009, nudge_y = 0, # ADJUST AS NEEDED
                  size = 3.5, fontface = "bold", family = "Arial", hjust = 1,
                  color = "black", bg.color = "white")+
  #Station labels - FWRI
  geom_shadowtext(data = Stations %>% filter(Source == "Oysters"), 
                  aes(Long, Lat, label = StationNam),
                  nudge_x = 0.009, nudge_y = 0, # ADJUST AS NEEDED
                  size = 3, fontface = "bold", family = "Arial", hjust = 0,
                  color = "black", bg.color = "white")+theme_classic()+
  scale_color_manual(values = c("#333333", "#D55E00"))+
  scale_shape_manual(values = c(16, 15))+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 12, color = "black"), 
        axis.text =  element_text(size = 10, color = "black"))+
  coord_sf(xlim = c(-80.15, -79.95),
           ylim = c(26.5, 26.85),  
           crs = 4326)
#1100 * ##
#
#
#
#### Output cleaned data ----
#
# Create workbook
wb <- createWorkbook()

# Add each data frame to its own worksheet
addWorksheet(wb, "Flow_df")
writeData(wb, "Flow_df", Flow_df)

addWorksheet(wb, "Sali_raw")
writeData(wb, "Sali_raw", Sali_raw)

addWorksheet(wb, "MeanSalinity")
writeData(wb, "MeanSalinity", MeanSalinity)

addWorksheet(wb, "Turb_raw")
writeData(wb, "Turb_raw", Turb_raw)

addWorksheet(wb, "MeanTurbidity")
writeData(wb, "MeanTurbidity", MeanTurbidity)

addWorksheet(wb, "Sediment_sta")
writeData(wb, "Sediment_sta", Sediment_sta)

addWorksheet(wb, "Sediment_sec")
writeData(wb, "Sediment_sec", Sediment_sec)

# Save workbook
saveWorkbook(
  wb,
  file = "LWL_WQ_Data.xlsx",
  overwrite = TRUE
)
#
rm(wb, Flow_raw, Salinity, Sedi_raw, WQ_raw)
#
#
#
#
#### Themes and functions ----
#
#
base_theme <- ggplot2::theme_classic() +
  ggplot2::theme(
    axis.title = element_text(size = 20, face = "bold", color = "black", family = "Arial"),
    axis.text = ggplot2::element_text(size = 18, family = "Arial", color = "black"),
    axis.text.x = element_text(margin = margin(t=0.25, r=0.5, b=0, l=0.5, unit = "cm")), #unit(c(0.25, 0.5, 0, 0.5), "cm")), 
    axis.text.y = element_text(margin = margin(t=0, r=0.35, b=0, l=0, unit = "cm")), #unit(c(0, 0.25, 0, 0), "cm")),
    axis.ticks = element_line(color = "black", linewidth = 0.1),
    axis.ticks.length = unit(-0.15, "cm"),
    panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.1),
    plot.margin = grid::unit(c(0.05, 0.05, 0.05, 0.05), "cm"),
    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5), family = "Arial"),
    plot.caption = ggplot2::element_text(face = "italic", size = 9),
    legend.title = element_text(size = 12, family = "Arial"),
    legend.text = element_text(size = 10, family = "Arial"))
#
scales_cont <- list(
  scale_x_continuous(limits = c(0, NA), 
                     breaks = pretty_breaks(n = 4),
                     expand = expansion(mult = c(0, 0.05))),
  scale_x_continuous(limits = c(0, NA), 
                     breaks = pretty_breaks(n = 4),
                     expand = expansion(mult = c(0, 0.05)))
)
#
#
# Hyperbolic model
fit_flow_models <- function(flow_data, associated_data, flow_col = "Mean_Flow", other_col = "Mean_Salinity"){
  # Assumptions:
  # - flow_data: data frame with columns for Date, Station, and specified flow_col
  # - associated_data: data frame with columns for Date, Station, and specified other_col
  # - The function pairs each associated data station with each flow station by matching on Date
  # - It performs an inner join on Date, so only matching time points are used
  # - Requires at least 4 data points for fitting (to avoid underdetermined models)
  station_col <- "Station"
  time_col <- "Date"
  # Get unique station IDs
  other_stations <- unique(associated_data[[station_col]])
  flow_stations <- unique(flow_data[[station_col]])
  # Initialize a list to store results
  results <- list()
  data_lookup <- list()
  
  # Loop over each combination
  for (other_station in other_stations) {
    for (flow_station in flow_stations) {
      # Subset salinity data for the current station
      other_sub <- associated_data %>%
        dplyr::filter(.data[[station_col]] == other_station) %>%
        dplyr::select(all_of(time_col), !!other_col := all_of(other_col))
      
      # Subset flow data for the current station
      flow_sub <- flow_data %>%
        dplyr::filter(.data[[station_col]] == flow_station) %>%
        dplyr::select(all_of(time_col), !!flow_col := all_of(flow_col))
      
      # Merge on time (inner join to get matching time points)
      combined <- inner_join(other_sub, flow_sub, by = time_col) %>% tidyr::drop_na()
      
      # Get model name
      model_name <- paste(
        stringr::str_replace_all(other_station, "_", ""),
        stringr::str_replace_all(flow_station, "_", ""),
        sep = "_"
      )
      
      # Store data_lookup table
      if (nrow(combined) > 0) {
        data_lookup[[model_name]] <- combined
      }
      
      # Check if there are enough data points (at least 4 for NLS)
      if (nrow(combined) >= 4) {
        #
        # Handling zero-flow days 
        if (any(combined[[flow_col]] == 0, na.rm = TRUE)) {
          combined[[flow_col]] <- combined[[flow_col]] + 0.001
        }
        # Build the formula dynamically
        formula_str <- paste0(other_col, " ~ y0 + (a * b) / (b + ", flow_col, ")")
        
        # Attempt to fit the model
        fit <- try(
          nlsLM(
            as.formula(formula_str),
            data = combined,
            start = list(
              # Handling extreme outliers using 1% and 99% quantiles
              y0 = quantile(combined[[other_col]], 0.01, na.rm = TRUE)[[1]], #min(combined[[other_col]], na.rm = TRUE),
              a  = quantile(combined[[other_col]], 0.99, na.rm = TRUE)[[1]] - quantile(combined[[other_col]], 0.01, na.rm = TRUE)[[1]], #max(combined[[other_col]], na.rm = TRUE) - min(combined[[other_col]], na.rm = TRUE),
              b  = median(combined[[flow_col]], na.rm = TRUE)
            )
          ),
          silent = TRUE
        )
        results[[model_name]] <- combined
        # Store the summary if fit succeeded, otherwise store an error message
        if (!inherits(fit, "try-error")) {
          results[[model_name]] <- summary(fit)
        } else {
          results[[model_name]] <- paste("Fit failed for data station", other_station, "and flow station", flow_station, ":", attr(fit, "condition")$message)
        }
      } else {
        results[[model_name]] <- paste("Insufficient data for data station", other_station, "and flow station", flow_station, "(only", nrow(combined), "matching time points)")
      }
    }
  }
  # Print the list of all model names (combinations) that were attempted
  print("Models attempted (otherStation_flowStation):")
  print(names(results))
  #assign("Model_data", results_data, envir = .GlobalEnv)
  return(list(models = results,
              data_lookup = data_lookup))
}
# Linear model
fit_flow_turb_models <- function(flow_data,
                                 associated_data,
                                 flow_col = "Mean_Flow",
                                 other_col = "Mean_Turbidity") {
  
  library(quantreg)
  
  station_col <- "Station"
  time_col <- "Date"
  
  # Get unique station IDs
  other_stations <- unique(associated_data[[station_col]])
  flow_stations  <- unique(flow_data[[station_col]])
  
  # Initialize lists
  results <- list()
  data_lookup <- list()
  
  # Loop over each station combination
  for (other_station in other_stations) {
    for (flow_station in flow_stations) {
      
      # Subset associated data
      other_sub <- associated_data %>%
        dplyr::filter(.data[[station_col]] == other_station) %>%
        dplyr::select(all_of(time_col), !!other_col := all_of(other_col))
      
      # Subset flow data
      flow_sub <- flow_data %>%
        dplyr::filter(.data[[station_col]] == flow_station) %>%
        dplyr::select(all_of(time_col), !!flow_col := all_of(flow_col))
      
      # Merge by date
      combined <- suppressWarnings(dplyr::inner_join(other_sub, flow_sub, by = time_col) %>%
        tidyr::drop_na() %>%
        mutate(logFlow = log10(.data[[flow_col]] + 1),
               logOther = log10(.data[[other_col]] + 1)))
      
      # Model name
      model_name <- paste(
        stringr::str_replace_all(other_station, "_", ""),
        stringr::str_replace_all(flow_station, "_", ""),
        sep = "_"
      )
      
      # Store merged data
      if (nrow(combined) > 0) {
        data_lookup[[model_name]] <- combined
      }
      
      # Need at least 2 observations to fit a linear model
      if (nrow(combined) >= 2) {
        
        # Fit model
        fit <- try(
          rq(logOther ~ logFlow + I(logFlow^2), tau = 0.95, data = combined),#lm(logOther ~ logFlow, data = combined),#lm(logOther ~ poly(logFlow, 2, raw = TRUE), data = combined),
          silent = TRUE
        )
        
        if (!inherits(fit, "try-error")) {
          results[[model_name]] <- summary(fit)
        } else {
          results[[model_name]] <- paste(
            "Fit failed for data station",
            other_station,
            "and flow station",
            flow_station,
            ":",
            attr(fit, "condition")$message
          )
        }
        
      } else {
        
        results[[model_name]] <- paste(
          "Insufficient data for data station",
          other_station,
          "and flow station",
          "(only",
          nrow(combined),
          "matching time points)"
        )
      }
    }
  }
  
  print("Models attempted (otherStation_flowStation):")
  print(names(results))
  
  return(list(
    models = results,
    data_lookup = data_lookup
  ))
}
# Update models:
filter_models <- function(results, models, mode = c("keep", "remove")) {
  #
  stopifnot(
    is.list(results),
    all(c("models", "data_lookup") %in% names(results))
  )
  #
  mode <- match.arg(mode)
  # Get model names
  model_names <- names(results$models)
  # Keep or remove models 
  if (mode == "keep") {
    valid_models <- intersect(models, model_names)
    
    if (length(valid_models) == 0) {
      stop("None of the specified models were found in results$models.")
    }
    
    if (length(valid_models) < length(models)) {
      warning("Some requested models were not found and were ignored.")
    }
    
    keep_names <- valid_models
    
  } else {  # mode == "remove"
    
    invalid_models <- setdiff(models, model_names)
    if (length(invalid_models) > 0) {
      warning("Some models to remove were not found and were ignored.")
    }
    
    keep_names <- setdiff(model_names, models)
  }
  # Output filtered data
  list(
    models      = results$models[keep_names],
    data_lookup = results$data_lookup[names(results$data_lookup) %in% keep_names]
  )
}
#
ggplot_hyperbolic_fit <- function(resultsdf, 
                                  results, 
                                  model_name,
                                  flow_col = "Flow",
                                  value_col = "Salinity",
                                  Salinity_min = NULL, Salinity_max = NULL,
                                  Flow_min = NULL, Flow_max = NULL) {
  #
  #
  make_plot <- function(model_name) {
    #
    fit <- results[[model_name]]
    #
    if (!(model_name %in% names(resultsdf))) {
      stop("Model name not found in results.")
    }
    # Skip models that failed to fit
    if (!inherits(fit, "summary.nls")) {
      message("Skipping ", model_name, " (fit unsuccessful).")
      return(NULL)
    }
    #
    df <- resultsdf[[model_name]]
    
    if (!all(c(flow_col, value_col) %in% names(df)))
      return(NULL)
    
    p <- coef(fit)
    y0 <- p[1]
    a  <- p[2]
    b  <- p[3]
    
    xseq <- seq(min(df[[flow_col]]),
                max(df[[flow_col]]),
                length.out = 300)
    
    pred_df <- data.frame(
      flow = xseq,
      fitted = y0 + (a * b)/(b + xseq)
    ) %>%
      rename(!!flow_col := flow)
    
    ggplot(df,
           aes(x = .data[[flow_col]],
               y = .data[[value_col]])) +
      {if(!is.null(Salinity_min) && !is.null(Salinity_max))
        annotate("rect",
                 xmin=-Inf,xmax=Inf,
                 ymin=Salinity_min,ymax=Salinity_max,
                 alpha=.6,fill="#B8FFB8")} +
      {if(!is.null(Flow_min) && !is.null(Flow_max))
        annotate("rect",
                 xmin=Flow_min,xmax=Flow_max,
                 ymin=-Inf,ymax=Inf,
                 alpha=.6,fill="#97FFFF")} +
      geom_point(color="gray30", size=2) +
      geom_line(data=pred_df,
                aes(x=.data[[flow_col]], y=fitted),
                color="blue",
                linewidth=1.2) +
      scale_y_continuous(expand=c(.005,.1)) +
      scale_x_continuous(expand=c(.005,.1)) +
      labs(
        title=model_name,
        subtitle=paste0(
          "y = ", round(y0,2),
          " + (", round(a,2),
          " × ", round(b,2),
          ")/(", round(b,2), " + x)"
        ),
        x=flow_col,
        y=value_col
      ) +
      theme_classic()
  }
  #
  ## HANDLE "all" FIRST
  ## =========================
  ## Return a grid of all models
  if (tolower(model_name) == "all") {
    
    plots <- lapply(names(resultsdf), make_plot)
    
    ## remove failed models
    plots <- plots[!sapply(plots, is.null)]
    
    n <- length(plots)
    
    if (n == 0)
      stop("No successful models to plot.")
    
    ncol <- ceiling(sqrt(n))
    nrow <- ceiling(n / ncol)
    
    return(
      patchwork::wrap_plots(
        plots,
        ncol = ncol,
        nrow = nrow
      )
    )
  }
  
  ## SINGLE MODEL PATH
  ## =========================
  if (!(model_name %in% names(resultsdf))) {
    stop("Model name not found in results.")
  }
  
  make_plot(model_name)
}
#
#
ggplot_quad_fit <- function(resultsdf, 
                            results, 
                            model_name,
                            flow_col = "Flow",
                            value_col = "Turbidity",
                            Value_min = NULL, Value_max = NULL,
                            Flow_min = NULL, Flow_max = NULL) {
  #
  #
  make_plot <- function(model_name) {
    #
    fit <- results[[model_name]]
    #
    if (!(model_name %in% names(resultsdf))) {
      stop("Model name not found in results.")
    }
    # Skip models that failed to fit
    if (!inherits(fit, "summary.rq")) {
      message("Skipping ", model_name, " (fit unsuccessful).")
      return(NULL)
    }
    #
    df <- resultsdf[[model_name]]
    df <- df %>%
      mutate(logFlow = log10(.data[[flow_col]] + 1),
             logOther = log10(.data[[value_col]] + 1))
    
    if (!all(c("logFlow", "logOther") %in% names(df)))#if (!all(c(flow_col, value_col) %in% names(df)))
      return(NULL)
    
    # Extract coefficients
    p <- coef(fit)
    y0 <- p[1]
    a  <- p[2]
    b  <- p[3]
    
    # Range of original data
    xseq <- seq(min(df[[flow_col]], na.rm = T),
                max(df[[flow_col]], na.rm = T),
                length.out = 1000)
    log_x <- log10(xseq + 1)
    
    # Predict on log+1 scale
    pred_df <- data.frame(
      Flow = xseq,
      logFlow = log_x,
      fittedLog = y0 + a * log_x + b * log_x^2 #fitted = y0 + (a * b)/(b + xseq)
    ) %>%
      mutate(Other = (10^fittedLog) - 1)
    
    ggplot(df,
           aes(x = .data[[flow_col]],
               y = .data[[value_col]])) +
      {if(!is.null(Value_min) && !is.null(Value_max))
        annotate("rect",
                 xmin=-Inf,xmax=Inf,
                 ymin=Value_min,ymax=Value_max,
                 alpha=.6,fill="#B8FFB8")} +
      {if(!is.null(Flow_min) && !is.null(Flow_max))
        annotate("rect",
                 xmin=Flow_min,xmax=Flow_max,
                 ymin=-Inf,ymax=Inf,
                 alpha=.6,fill="#97FFFF")} +
      geom_point(color="gray30", size=2) +
      geom_line(data=pred_df,
                aes(x=Flow, y=Other),#.data[[flow_col]], y=fitted),
                color="blue",
                linewidth=1.2) +
      scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
      scale_x_continuous(expand=expansion(mult=c(0,0.05))) +
      labs(
        title=model_name,
        subtitle=paste0("y = 10^[", round(y0, 2)," + (", round(a, 2), "*log(x))"," + (", round(b, 2), "*log(x)^2)] - 1"), #Normal scale
        #paste0("log(y) = ", round(y0,2)," + (", round(a,2),"*log(x)) + (", round(b,2), "*log(x)^2)"), #Log scale
        x=flow_col,
        y=value_col
      ) +
      theme_classic()
  }
  #
  ## HANDLE "all" FIRST
  ## =========================
  ## Return a grid of all models
  if (tolower(model_name) == "all") {
    
    plots <- lapply(names(resultsdf), make_plot)
    
    ## remove failed models
    plots <- plots[!sapply(plots, is.null)]
    
    n <- length(plots)
    
    if (n == 0)
      stop("No successful models to plot.")
    
    ncol <- ceiling(sqrt(n))
    nrow <- ceiling(n / ncol)
    
    return(
      patchwork::wrap_plots(
        plots,
        ncol = ncol,
        nrow = nrow
      )
    )
  }
  
  ## SINGLE MODEL PATH
  ## =========================
  if (!(model_name %in% names(resultsdf))) {
    stop("Model name not found in results.")
  }
  
  make_plot(model_name)
}
#
#
# Calculate flow at specified salinity (from HSM curves)
flow_at_salinity_hyp2 <- function(results, target_sal, data_lookup) {
  # results: output from fit_salinity_flow_models (list of summaries or error messages)
  # target_sal: target salinity value
  # data_lookup: named list of data frames used in each model
  #              must include columns: salinity, flow
  
  # Initialize a data frame to store results
  flow_results <- data.frame(
    salinity_station = character(),
    flow_station = character(),
    flow_min         = numeric(),
    flow_max         = numeric(),
    flow_at_target = numeric(),
    status = character(),
    stringsAsFactors = FALSE
  )
  # Loop through each result
  for (model_name in names(results)) {
    result <- results[[model_name]]
    
    # Split the model name to get salinity and flow stations (assuming format: sal_station_flow_station)
    parts <- str_split(model_name, "_", n = 2)[[1]]
    sal_station_clean <- parts[1]
    flow_station_clean <- parts[2]
    
    flow_min <- NA
    flow_max <- NA
    flow_val <- NA
    status   <- "Failed fit"
    #
    # Successful fit ----
    if (inherits(result, "summary.nls")) {
      # Successful fit: extract coefficients and compute flow
      p <- coef(result)
      y0 <- p[1]
      a  <- p[2]
      b  <- p[3]
      
      if (target_sal <= y0) {
        status <- "Target salinity <= y0"
      } else {
        flow_val <- (a * b) / (target_sal - y0) - b
        status <- "Success"
      }
    }
    #
    # All flow in ideal salinity range ----
    if (is.na(flow_val) && model_name %in% names(data_lookup)) {
      dat <- data_lookup[[model_name]]
      
      if (target_sal <= min(dat$Mean_Salinity, na.rm = TRUE) ||
          target_sal >= max(dat$Mean_Salinity, na.rm = TRUE)) {
        
        flow_min <- min(dat$Mean_Flow, na.rm = TRUE)
        flow_max <- max(dat$Mean_Flow, na.rm = TRUE)
        status   <- "All flows within target range; returning flow bounds"
        
        if(target_sal <= min(dat$Mean_Salinity, na.rm = TRUE)){
          flow_val <- flow_max
        } else if(target_sal >= max(dat$Mean_Salinity, na.rm = TRUE)){
          flow_val <- flow_min
        }
      }
    }
    # Append to results data frame ----
    flow_results <- rbind(
      flow_results, 
      data.frame(
        salinity_station = sal_station_clean,
        flow_station = flow_station_clean,
        flow_min = flow_min,
        flow_max = flow_max,
        flow_at_target = flow_val,
        status = status,
        stringsAsFactors = FALSE
      ))
  }
  return(flow_results)
}
#
extract_model_coefficients <- function(models, sep = "_") {

  coef_df <- imap_dfr(models, function(model, model_name) {
    
    # Split model name
    stations <- strsplit(model_name, sep, fixed = TRUE)[[1]]

    # Convert coefficient vector to one-row data frame
    coefs <- as.data.frame(model$coefficients) %>% rownames_to_column()
    
    
    bind_cols(
      tibble(
        salinity_station = stations[1],
        flow_station = stations[2]
      ),
      coefs
    )
  })
  
  coef_df
}
#
#
#
#
#### Curves F vs salinity ----
#
### Flow vs salinity
#
## Daily flow vs daily salinity
flow_t <- Flow_df %>% 
  dplyr::select(Analysis_Date, Estuary, Date, S155_Flow, S41_Flow, S44_Flow) %>%
  pivot_longer(cols = c(S155_Flow, S41_Flow, S44_Flow), names_to = c("Station", ".value"), names_pattern = "(.*)_(.*)")
sal_t <- Sali_raw %>%
  pivot_longer(cols = c(LWL20_Sali, LWL19_Sali), names_to = c("Station", ".value"), names_pattern = "(.*)_(.*)")
models_fs_d <- fit_flow_models(flow_data = flow_t, 
                          associated_data = sal_t,
                          flow_col = "Flow",
                          other_col = "Sali")
#Limit to desired models
models_fs_d <- filter_models(models_fs_d, c("LWL20_S41", "LWL19_S44"), mode = "remove")
#Plot models
(p <- ggplot_hyperbolic_fit(resultsdf = models_fs_d$data_lookup,
                      results = models_fs_d$models,
                      model_name = "all",
                      flow_col = "Flow",
                      value_col = "Sali"))
#
if(runANDsavePlots == "Y"){
  # Save combined plot:
  ggsave(
    filename = paste0("PBC/Output/Flow/FLow_salinity_daily_curves_",Sys.Date(),".png"),
    plot = p, width = 9, height = 5, units = "in", dpi = 300)
  #
  # Save indivudal plots:
  for (m_name in names(models_fs_d$models)) {
    
    p2 <- ggplot_hyperbolic_fit(resultsdf = models_fs_d$data_lookup,
                                results = models_fs_d$models,
                                model_name = m_name,
                                flow_col = "Flow",
                                value_col = "Sali")
    plot2 <- p2 + 
      base_theme +
      scales_cont +
      labs(x = "Flow (cfs)",
           y = "Salinity")
    
    ggsave(
      filename = paste0("PBC/Output/Flow/FLow_salinity_daily_",m_name,"_",Sys.Date(),".png"),
      plot = plot2, width = 9, height = 5, units = "in", dpi = 300)
  }
} else {
  message("Manually save plots using existing code or update 'runANDsavePlots' to 'Y'.")
}
#
#Optimal flow ranges & model formulas
fs_daily_range <- rbind(
  flow_at_salinity_hyp2(models_fs_d$models, 10, models_fs_d$data_lookup) %>% mutate(Sal = "min", Flow = "max"), 
  flow_at_salinity_hyp2(models_fs_d$models, 25, models_fs_d$data_lookup) %>% mutate(Sal = "max", Flow = "min")) %>%
  left_join(
    extract_model_coefficients(models_fs_d$models))
#
fs_daily_range %>% 
  dplyr::select(salinity_station, flow_station, Sal, Flow, flow_at_target) %>% distinct()
#
#
##
##
#
#
## 7day and 14day flow vs daily salinity
flow_t7 <- Flow_df %>% 
  dplyr::select(Analysis_Date, Estuary, Date, Sum7, Sum14, Sum7_155, Sum14_155) %>%
  pivot_longer(cols = c(Sum7, Sum14, Sum7_155, Sum14_155), names_to = "Station", values_to = "Flow")
models_fs_d7 <- fit_flow_models(flow_data = flow_t7, 
                               associated_data = sal_t,
                               flow_col = "Flow",
                               other_col = "Sali")
#Plot models
(p2 <- ggplot_hyperbolic_fit(resultsdf = models_fs_d7$data_lookup,
                            results = models_fs_d7$models,
                            model_name = "all",
                            flow_col = "Flow",
                            value_col = "Sali"))
#
if(runANDsavePlots == "Y"){
  ggsave(
    filename = paste0("PBC/Output/Flow/Flow_salinity_7d_14d_curves_",Sys.Date(),".png"),
    plot = p2, width = 9, height = 5, units = "in", dpi = 300)
  #
  for (m_name in names(models_fs_d7$models)) {
    p2 <- ggplot_hyperbolic_fit(resultsdf = models_fs_d7$data_lookup,
                                results = models_fs_d7$models,
                                model_name = m_name,
                                flow_col = "Flow",
                                value_col = "Sali")
    plot2 <- p2 + 
      base_theme +
      scales_cont +
      labs(x = "Flow (cfs)",
           y = "Salinity")
    ggsave(
      filename = paste0("PBC/Output/Flow/Flow_salinity_",m_name,"_",Sys.Date(),".png"),
      plot = plot2, width = 9, height = 5, units = "in", dpi = 300)
  }
} else {
  message("Manually save plots using existing code or update 'runANDsavePlots' to 'Y'.")
}
#
#Optimal flow ranges & model formulas
fs_714_range <- rbind(
  flow_at_salinity_hyp2(models_fs_d7$models, 10, models_fs_d7$data_lookup) %>% mutate(Sal = "min", Flow = "max"), 
  flow_at_salinity_hyp2(models_fs_d7$models, 25, models_fs_d7$data_lookup) %>% mutate(Sal = "max", Flow = "min")) %>%
  left_join(
    extract_model_coefficients(models_fs_d7$models))
#
fs_714_range %>% 
  dplyr::select(salinity_station, flow_station, Sal, Flow, flow_at_target) %>% distinct()
#
#
##
##
#
#
## Monthly flow vs monthly salinity
flow_tm <- Flow_df %>% 
  dplyr::select(Analysis_Date, Estuary, S155_Flow, S41_Flow, S44_Flow) %>%
  pivot_longer(cols = c(S155_Flow, S41_Flow, S44_Flow), names_to = c("Station", ".value"), names_pattern = "(.*)_(.*)") %>%
    group_by(Analysis_Date, Station) %>%
  summarise(Flow = sum(Flow, na.rm = T)) %>%
  rename(Date = Analysis_Date)
sal_tm <- MeanSalinity %>%
  pivot_longer(cols = c(-AnalysisDate, -Estuary), names_to = c("Station"), values_to = "Sali") %>%
  rename(Date = AnalysisDate) 
models_fs_m <- fit_flow_models(flow_data = flow_tm, 
                                associated_data = sal_tm,
                                flow_col = "Flow",
                                other_col = "Sali")
#Limit to desired models
models_fs_m <- filter_models(models_fs_m, c("LW-R4_S44", "LWL19_S155","LWL20_S155", "LW-L1_S155",
                                            "LW-L2_S155", "LW-L3_S155", "LW-R2_S155", "LW-R3_S155",
                                            "LW-R4_S155", "SC_S155", "NC_S155", "LW-L3_S41",
                                            "NC_S44", "SC_S41"), 
                             mode = "keep")
#Plot models
(p3 <- ggplot_hyperbolic_fit(resultsdf = models_fs_m$data_lookup,
                            results = models_fs_m$models,
                            model_name = "all",
                            flow_col = "Flow",
                            value_col = "Sali"))
#
if(runANDsavePlots == "Y"){
  ggsave(
    filename = paste0("PBC/Output/Flow/Flow_salinity_monthly_curves_",Sys.Date(),".png"),
    plot = p3, width = 9, height = 5, units = "in", dpi = 300)
  #
  for (m_name in names(models_fs_m$models)) {
    p2 <- ggplot_hyperbolic_fit(resultsdf = models_fs_m$data_lookup,
                                results = models_fs_m$models,
                                model_name = m_name,
                                flow_col = "Flow",
                                value_col = "Sali")
    plot2 <- p2 + 
      base_theme +
      scales_cont +
      labs(x = "Flow (cfs)",
           y = "Salinity")
    ggsave(
      filename = paste0("PBC/Output/Flow/Flow_salinity_monthly_",m_name,"_",Sys.Date(),".png"),
      plot = plot2, width = 9, height = 5, units = "in", dpi = 300)
  }
} else {
  message("Manually save plots using existing code or update 'runANDsavePlots' to 'Y'.")
}
#
#Optimal flow ranges & model formulas
fs_m_range <- rbind(
  flow_at_salinity_hyp2(models_fs_m$models, 10, models_fs_m$data_lookup) %>% mutate(Sal = "min", Flow = "max"), 
  flow_at_salinity_hyp2(models_fs_m$models, 25, models_fs_m$data_lookup) %>% mutate(Sal = "max", Flow = "min")) %>%
  left_join(
    extract_model_coefficients(models_fs_m$models))
#
fs_m_range %>% 
  dplyr::select(salinity_station, flow_station, Sal, Flow, flow_at_target) %>% distinct()
#
#
#
#
#
#### Curves F vs turbidity ----
#
### Flow vs turbidity
#
## Daily flow vs daily turbidity
head(flow_t)
#flow_t <- Flow_df %>% dplyr::select(Analysis_Date, Estuary, Date, S155_Flow, S41_Flow, S44_Flow) %>% pivot_longer(cols = c(S155_Flow, S41_Flow, S44_Flow), names_to = c("Station", ".value"), names_pattern = "(.*)_(.*)")
turb_t <- Turb_raw %>% dplyr::select(-LWMean) %>%
  pivot_longer(cols = c(LWL20_Turb, LWL19_Turb), names_to = c("Station", ".value"), names_pattern = "(.*)_(.*)")
#
left_join(flow_t %>% filter(Station == "S155") %>% group_by(Date) %>% summarise(Flow = sum(Flow, na.rm = T)), 
          turb_t %>% filter(Station == "LWL19")%>% group_by(Date) %>% summarise(Turb = mean(Turb, na.rm = T))) %>%
  ggplot(aes(Flow, Turb))+ geom_point()
#
models_ft_d <- fit_flow_turb_models(flow_data = flow_t, 
                               associated_data = turb_t,
                               flow_col = "Flow",
                               other_col = "Turb")
#Limit to desired models
models_ft_d <- filter_models(models_ft_d, c("LWL20_S41", "LWL20_S44", "LWL19_S41", "LWL19_S44"), mode = "remove")
names(models_ft_d$models)
#Plot models
(p4 <- ggplot_quad_fit(resultsdf = models_ft_d$data_lookup,
                       results = models_ft_d$models,
                       model_name = "all",
                       flow_col = "Flow",
                       value_col = "Turb"))
#
ft_daily_model <- extract_model_coefficients(models_ft_d$models)
#
if(runANDsavePlots == "Y"){
  # Save combined plot:
  ggsave(
    filename = paste0("PBC/Output/Flow/Flow_turbidity_daily_curves_",Sys.Date(),".png"),
    plot = p4, width = 9, height = 5, units = "in", dpi = 300)
  #
  # Save indivudal plots:
  for (m_name in names(models_ft_d$models)) {
    
    p2 <- ggplot_quad_fit(resultsdf = models_ft_d$data_lookup,
                                results = models_ft_d$models,
                                model_name = m_name,
                                flow_col = "Flow",
                                value_col = "Turb")
    plot2 <- p2 + 
      base_theme +
      scales_cont +
      labs(x = "Flow (cfs)",
           y = "Turbidity (NTU)")
    
    ggsave(
      filename = paste0("PBC/Output/Flow/Flow_turbidity_daily_",m_name,"_",Sys.Date(),".png"),
      plot = plot2, width = 9, height = 5, units = "in", dpi = 300)
  }
} else {
  message("Manually save plots using existing code or update 'runANDsavePlots' to 'Y'.")
}
#
#
#
##
##
#
#
## Daily flow lag1 and lag2 vs daily turbidity
flow_tlag <- flow_t %>% 
  mutate(Flow1 = lag(Flow, n = 1),
         Flow2 = lag(Flow, n = 2))
head(turb_t)
#turb_t <- Turb_raw %>% dplyr::select(-LWMean) %>% pivot_longer(cols = c(LWL20_Turb, LWL19_Turb), names_to = c("Station", ".value"), names_pattern = "(.*)_(.*)")
#
#
models_ft_dlag <- fit_flow_turb_models(flow_data = flow_tlag %>% drop_na(), 
                                    associated_data = turb_t,
                                    flow_col = "Flow1",
                                    other_col = "Turb")
#Limit to desired models
models_ft_dlag <- filter_models(models_ft_dlag, c("LWL20_S41", "LWL20_S44", "LWL19_S41", "LWL19_S44"), mode = "remove")
names(models_ft_dlag$models)
#Plot models
(p5 <- ggplot_quad_fit(resultsdf = models_ft_dlag$data_lookup,
                       results = models_ft_dlag$models,
                       model_name = "all",
                       flow_col = "Flow1",
                       value_col = "Turb"))
#
ft_dayLag1_model <- extract_model_coefficients(models_ft_dlag$models)
#
if(runANDsavePlots == "Y"){
  # Save combined plot:
  ggsave(
    filename = paste0("PBC/Output/Flow/Flow_turbidity_dayLag1_curves_",Sys.Date(),".png"),
    plot = p5, width = 9, height = 5, units = "in", dpi = 300)
  #
  # Save indivudal plots:
  for (m_name in names(models_ft_dlag$models)) {
    
    p2 <- ggplot_quad_fit(resultsdf = models_ft_dlag$data_lookup,
                          results = models_ft_dlag$models,
                          model_name = m_name,
                          flow_col = "Flow1",
                          value_col = "Turb")
    plot2 <- p2 + 
      base_theme +
      scales_cont +
      labs(x = "Flow lag 1 day(cfs)",
           y = "Turbidity (NTU)")
    
    ggsave(
      filename = paste0("PBC/Output/Flow/Flow_turbidity_dayLag1_",m_name,"_",Sys.Date(),".png"),
      plot = plot2, width = 9, height = 5, units = "in", dpi = 300)
  }
} else {
  message("Manually save plots using existing code or update 'runANDsavePlots' to 'Y'.")
}
#
#
models_ft_d2lag <- fit_flow_turb_models(flow_data = flow_tlag %>% drop_na(), 
                                       associated_data = turb_t,
                                       flow_col = "Flow2",
                                       other_col = "Turb")
#Limit to desired models
models_ft_d2lag <- filter_models(models_ft_d2lag, c("LWL20_S41", "LWL20_S44", "LWL19_S41", "LWL19_S44"), mode = "remove")
names(models_ft_d2lag$models)
#Plot models
(p6 <- ggplot_quad_fit(resultsdf = models_ft_d2lag$data_lookup,
                       results = models_ft_d2lag$models,
                       model_name = "all",
                       flow_col = "Flow2",
                       value_col = "Turb"))
#
ft_dayLag2_model <- extract_model_coefficients(models_ft_d2lag$models)
#
if(runANDsavePlots == "Y"){
  # Save combined plot:
  ggsave(
    filename = paste0("PBC/Output/Flow/Flow_turbidity_dayLag2_curves_",Sys.Date(),".png"),
    plot = p6, width = 9, height = 5, units = "in", dpi = 300)
  #
  # Save indivudal plots:
  for (m_name in names(models_ft_d2lag$models)) {
    
    p2 <- ggplot_quad_fit(resultsdf = models_ft_d2lag$data_lookup,
                          results = models_ft_d2lag$models,
                          model_name = m_name,
                          flow_col = "Flow2",
                          value_col = "Turb")
    plot2 <- p2 + 
      base_theme +
      scales_cont +
      labs(x = "Flow lag 2 day(cfs)",
           y = "Turbidity (NTU)")
    
    ggsave(
      filename = paste0("PBC/Output/Flow/Flow_turbidity_dayLag2_",m_name,"_",Sys.Date(),".png"),
      plot = plot2, width = 9, height = 5, units = "in", dpi = 300)
  }
} else {
  message("Manually save plots using existing code or update 'runANDsavePlots' to 'Y'.")
}
#
#
#
##
##
#
#
## 7day and 14day flow vs daily turbidity 
head(flow_t7)
#flow_t7 <- Flow_df %>% dplyr::select(Analysis_Date, Estuary, Date, Sum7, Sum14, Sum7_155, Sum14_155) %>% pivot_longer(cols = c(Sum7, Sum14, Sum7_155, Sum14_155), names_to = "Station", values_to = "Flow")
models_ft_d7 <- fit_flow_turb_models(flow_data = flow_t7, 
                                associated_data = turb_t,
                                flow_col = "Flow",
                                other_col = "Turb")
#
#Plot models 
(p7 <- ggplot_quad_fit(resultsdf = models_ft_d7$data_lookup,
                       results = models_ft_d7$models,
                       model_name = "all",
                       flow_col = "Flow",
                       value_col = "Turb"))
#
if(runANDsavePlots == "Y"){
  ggsave(
    filename = paste0("PBC/Output/Flow/Flow_turbidity_7d_14d_curves_",Sys.Date(),".png"),
    plot = p2, width = 9, height = 5, units = "in", dpi = 300)
  #
  for (m_name in names(models_ft_d7$models)) {
    p2 <- ggplot_hyperbolic_fit(resultsdf = models_ft_d7$data_lookup,
                                results = models_ft_d7$models,
                                model_name = m_name,
                                flow_col = "Flow",
                                value_col = "Turb")
    plot2 <- p2 + 
      base_theme +
      scales_cont +
      labs(x = "Flow (cfs)",
           y = "Turbidity (NTU)")
    ggsave(
      filename = paste0("PBC/Output/Flow/Flow_turbidity_",m_name,"_",Sys.Date(),".png"),
      plot = plot2, width = 9, height = 5, units = "in", dpi = 300)
  }
} else {
  message("Manually save plots using existing code or update 'runANDsavePlots' to 'Y'.")
}
#
#Optimal flow ranges & model formulas
ft_714_range <- extract_model_coefficients(models_ft_d7$models)
#
#
##
##
#
###### WORKING - STOPPING POINT 
## Monthly flow vs monthly salinity
flow_tm <- Flow_df %>% 
  dplyr::select(Analysis_Date, Estuary, S155_Flow, S41_Flow, S44_Flow) %>%
  pivot_longer(cols = c(S155_Flow, S41_Flow, S44_Flow), names_to = c("Station", ".value"), names_pattern = "(.*)_(.*)") %>%
  group_by(Analysis_Date, Station) %>%
  summarise(Flow = sum(Flow, na.rm = T)) %>%
  rename(Date = Analysis_Date)
sal_tm <- MeanSalinity %>%
  pivot_longer(cols = c(-AnalysisDate, -Estuary), names_to = c("Station"), values_to = "Sali") %>%
  rename(Date = AnalysisDate) 
models_fs_m <- fit_flow_models(flow_data = flow_tm, 
                               associated_data = sal_tm,
                               flow_col = "Flow",
                               other_col = "Sali")
#Limit to desired models
models_fs_m <- filter_models(models_fs_m, c("LW-R4_S44", "LWL19_S155","LWL20_S155", "LW-L1_S155",
                                            "LW-L2_S155", "LW-L3_S155", "LW-R2_S155", "LW-R3_S155",
                                            "LW-R4_S155", "SC_S155", "NC_S155", "LW-L3_S41",
                                            "NC_S44", "SC_S41"), 
                             mode = "keep")
#Plot models
(p3 <- ggplot_hyperbolic_fit(resultsdf = models_fs_m$data_lookup,
                             results = models_fs_m$models,
                             model_name = "all",
                             flow_col = "Flow",
                             value_col = "Sali"))
#
if(runANDsavePlots == "Y"){
  ggsave(
    filename = paste0("PBC/Output/Flow/Flow_salinity_monthly_curves_",Sys.Date(),".png"),
    plot = p3, width = 9, height = 5, units = "in", dpi = 300)
  #
  for (m_name in names(models_fs_m$models)) {
    p2 <- ggplot_hyperbolic_fit(resultsdf = models_fs_m$data_lookup,
                                results = models_fs_m$models,
                                model_name = m_name,
                                flow_col = "Flow",
                                value_col = "Sali")
    plot2 <- p2 + 
      base_theme +
      scales_cont +
      labs(x = "Flow (cfs)",
           y = "Salinity")
    ggsave(
      filename = paste0("PBC/Output/Flow/Flow_salinity_monthly_",m_name,"_",Sys.Date(),".png"),
      plot = plot2, width = 9, height = 5, units = "in", dpi = 300)
  }
} else {
  message("Manually save plots using existing code or update 'runANDsavePlots' to 'Y'.")
}
#
#Optimal flow ranges & model formulas
fs_m_range <- rbind(
  flow_at_salinity_hyp2(models_fs_m$models, 10, models_fs_m$data_lookup) %>% mutate(Sal = "min", Flow = "max"), 
  flow_at_salinity_hyp2(models_fs_m$models, 25, models_fs_m$data_lookup) %>% mutate(Sal = "max", Flow = "min")) %>%
  left_join(
    extract_model_coefficients(models_fs_m$models))
#
fs_m_range %>% 
  dplyr::select(salinity_station, flow_station, Sal, Flow, flow_at_target) %>% distinct()
#
#
#
#
#
#### Output summary and results data ----
#
#
# Create workbook
wb <- createWorkbook()

# Add each data frame to its own worksheet
addWorksheet(wb, "F_sal_daily")
writeData(wb, "F_sal_daily", fs_daily_range)

addWorksheet(wb, "F_sal_7d14d")
writeData(wb, "F_sal_7d14d", fs_m_range)

addWorksheet(wb, "F_sal_monthly")
writeData(wb, "F_sal_monthly", fs_m_range)

addWorksheet(wb, "F_turb_daily")
writeData(wb, "F_turb_daily", ft_daily_model)

addWorksheet(wb, "F_turb_lag1")
writeData(wb, "F_turb_lag1", ft_dayLag1_model)

addWorksheet(wb, "F_turb_lag2")
writeData(wb, "F_turb_lag2", ft_dayLag2_model)

addWorksheet(wb, "F_turb_7d14d")
writeData(wb, "F_turb_7d14d", ft_714_range)

# Save workbook
saveWorkbook(
  wb,
  file = "LWL_Flow_Summary.xlsx",
  overwrite = TRUE
)