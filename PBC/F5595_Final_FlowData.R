##F5595 Final - Flow data relationships
#
#
pacman::p_load(odbc, DBI, dbplyr,
               tidyverse, dplyr,  stringr, #DF manipulation
               DT, openxlsx, readxl,        #Excel
               lubridate, zoo,         #Dates
               slider, #rolling flow
               minpack.lm, #curve fit
               glmmTMB, purrr, emmeans, car, broom, multcomp, #Analyses
               knitr, kableExtra, scales, gt, gtExtras,
               install = TRUE)
#
#
#### Set up ----
#
Start_date <- as.Date("2023-10-01")
End_date <- as.Date("2026-09-30")
Database <- "Oysters_26-06-01"  #Set the local database to use
Server <- "localhost\\ERICALOCALSQL" #Set the local Server to use
Estuaries <- c("LW")
#
#
#
#### Data files ----
#
## Stations
Stations <- readWorkbook("PBC/Data/F5595_Stations.xlsx", sheet = "Sheet1", detectDates = TRUE, check.names = TRUE)
#
## DBHYDRO
Flow_raw <- readWorkbook("DBHYDRO/Shared_data/LW_Flow.xlsx", sheet = paste0(Estuaries, 'Sum'), detectDates = TRUE, check.names = TRUE) %>%
  filter(Date >= Start_date & Date <= End_date)
#
Flow_df <- Flow_raw %>% 
  dplyr::select(-LWSum) %>%
  pivot_longer(cols = c(S155_Flow, S41_Flow, S44_Flow),
               names_to = c("Station", ".value"),
               names_pattern = "(.*)_(.*)") %>%
  group_by(Station) %>%
  mutate(Roll7DaySum = slide_index_dbl(
    .x = Flow, 
    .i = Date, 
    .f = sum, 
    .before = days(6) # Current day + 6 days prior = 7 days total
  )) %>%
  ungroup()
#
Rain_raw <- readWorkbook("DBHYDRO/Shared_data/LW_Rain.xlsx", sheet = paste0(Estuaries, 'RainSum'), detectDates = TRUE, check.names = TRUE) %>%
  filter(Date >= Start_date & Date <= End_date)
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
           AnalysisDate = as.Date(floor_date(RetDate, unit = "month"))) %>%
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
           AnalysisDate = as.Date(floor_date(RetDate, unit = "month")),
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
## Clean and combine
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
#
#
#
#### Station map ----
#
ggplot()+
  #geom_sf(data = Site_area, fill = "#99CCFF")+
  #geom_sf(data = Site_Grid, fill = NA)+
  #geom_sf(data = FL_outline)+
  #Individual station points if grouping:
  #geom_point(data = salinity_raw, aes(Longitude, Latitude),  color = "#666666", shape = 8, size = 4)+
  geom_point(data = Stations, aes(Long, Lat,  color = Source, shape = Source), alpha = 0.8, size = 4)+
  theme_classic()+
  scale_color_manual(values = c("#333333", "#D55E00"))+
  scale_shape_manual(values = c(16, 15))+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 12, color = "black"), 
        axis.text =  element_text(size = 10, color = "black"))+
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"]-0.05, st_bbox(Site_area)["xmax"]+0.15),
           ylim = c(st_bbox(Site_area)["ymin"]-0.05, st_bbox(Site_area)["ymax"]+0.05))
#
#
#
#
#### Curve function ----
#
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
  browser()
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
#
#
models <- fit_flow_models(flow_data = Flow_df, 
                          associated_data = Salinity,
                          flow_col = "Flow",
                          other_col = "Sal")
#
names(models$models)
#
#
ggplot_hyperbolic_fit <- function(resultsdf, results, model_name, flow_col = "Flow", value_col = "Other", 
                                  Other_min = NULL, Other_max = NULL, Flow_min = NULL, Flow_max = NULL) {
  df <- resultsdf[[model_name]]
  # Check if the model exists and is successful
  if (!(model_name %in% names(resultsdf))) {
    stop("Model name not found in results.")
  }
  fit <- results[[model_name]]
  # Input validation
  if (!all(c(flow_col, value_col) %in% names(df))) {
    stop("Data frame must contain the specified flow and value columns.")
  }
  coefs <- coef(fit)
  if (!is.numeric(df[[flow_col]]) || !is.numeric(df[[value_col]])) {
    stop("Specified columns must be numeric.")
  }
  # Extract parameters
  p <- coef(fit)
  y0 <- p[1]
  a  <- p[2]
  b  <- p[3]
  
  # Build prediction grid
  xseq <- seq(min(df[[flow_col]]), max(df[[flow_col]]), length.out = 300)
  pred <- y0 + (a * b) / (b + xseq)
  
  pred_df <- data.frame(
    flow = xseq,
    fitted = pred
  ) %>% rename(!!flow_col := flow)
  
  ggplot(df, aes(x = .data[[flow_col]], y = .data[[value_col]])) +
    {if(!is.null(Other_min) && !is.null(Other_max)) annotate("rect", xmin=-Inf, xmax=Inf, ymin=Other_min, ymax=Other_max, alpha=0.6, fill="#B8FFB8")}+
    {if(!is.null(Flow_min) && !is.null(Flow_max)) annotate("rect", xmin=Flow_min, xmax=Flow_max, ymin=-Inf, ymax=Inf, alpha=0.6, fill="#97FFFF")}+
    geom_point(color = "gray30", size = 2) +
    geom_line(data = pred_df,
              aes(x = .data[[flow_col]], y = fitted),
              color = "blue", linewidth = 1.2) +
    scale_y_continuous(expand = c(0.005,0.1))+ scale_x_continuous(expand = c(0.005,0.1))+
    labs(
      x = flow_col,
      y = value_col,
      title = paste(model_name),
      subtitle = paste0("Hyperbolic Fit: y =",round(y0,2), " + (", round(a,2), "*", round(b,2), ")/(",round(b,2)," + x)", collapse = "")
    ) +
    theme_classic()
}
#
#
ggplot_hyperbolic_fit(resultsdf = models$data_lookup,
                      results = models$models,
                      model_name = "LW-R4_S155",
                      flow_col = "Flow",
                      value_col = "Sal")
#