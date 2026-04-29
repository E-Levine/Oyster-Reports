# Create all Functions necessary for Report

### Filter Functions ###
# Survey
FilterFunction1 <- function(data) {
  data %>% 
    mutate(TripDate = as.Date(substring(QuadratID, 8, 15), format = "%Y%m%d"),
           FixedLocationID = substring(QuadratID, 19, 22),
           Year = year(TripDate)) %>%
    left_join(FixedLocations1, by = c("FixedLocationID")) %>%
    mutate(TimeSinceCultched = as.numeric(interval(DateLastCultched, TripDate), "years")) %>%
    mutate(ProjectGroup = case_when(
      grepl("RESTORE", StationName) ~ "RESTORE-2017",
      grepl("NFWF", StationName) & Year < 2019 ~ "NFWF-2015",
      grepl("SBM Hotel", StationName) | grepl("SBM Bulkhead", StationName) ~ "Historic Uncultched",
      grepl("SBM", StationName) & Year >= 2021 & TimeSinceCultched > 0 ~ "NFWF-2021",
      TRUE ~ "Historic Uncultched")) 
}
###

### Filter Functions ###
# Oyster Health (CI, Dermo, Repro)
FilterFunction2 <- function(data) {
  data %>% 
    mutate(TripDate = as.Date(substring(SampleEventID, 8, 15), format = "%Y%m%d"),
           FixedLocationID = substring(SampleEventID, 19, 22),
           Year = year(TripDate)) %>%
    left_join(FixedLocations1, by = c("FixedLocationID")) 
}
###

### Survey Functions ###
summarise_ALL <- function(data) {
  summarised_data <- data %>%
    summarise(NumLiveMean_raw = mean(NumLive, na.rm = TRUE),
              NumLiveSD_raw = sd(NumLive, na.rm = TRUE),
              NumDrillsMean_raw = mean(NumDrills, na.rm = TRUE),
              NumDrillsSD_raw = sd(NumDrills, na.rm = TRUE),
              TotalWeightMean_raw = mean(TotalWeight, na.rm = TRUE),
              TotalWeightSD_raw = sd(TotalWeight, na.rm = TRUE)) %>%
    mutate(NumLiveDensityMean_raw = NumLiveMean_raw * 4,
           NumLiveDensitySD_raw = NumLiveSD_raw * 4,
           NumDrillDensityMean_raw = NumDrillsMean_raw * 4,
           NumDrillDensitySD_raw = NumDrillsSD_raw * 4,
           TotalWeightDensityMean_raw = TotalWeightMean_raw * 4,
           TotalWeightDensitySD_raw = TotalWeightSD_raw * 4)
  return(summarised_data)
}


###


### Plot Functions
  # Function to generate grouped bar plots with SD bars
  # df is short for data frame
generate_grouped_plot <- function(df, x_variable, x_label, y_variable, y_label, y_SD, UseColor, UseFill, title) {
  
  df <- df %>%
    mutate(Upper = !!sym(y_variable) + !!sym(y_SD))
  max_upper <- max(ungroup(df) %>%
                     dplyr::select(Upper), na.rm = TRUE)
  
  plot_list <- df %>% 
    ggplot(aes(x = factor(!!sym(x_variable), levels = unique(!!sym(x_variable))), 
               y = !!sym(y_variable), 
               color = !!sym(UseColor), 
               fill = as.factor(!!sym(UseFill)))) + 
    geom_bar(stat = "identity", 
             position = position_dodge(width = 0.9),
             color = "black") + 
    geom_errorbar(aes(ymin = !!sym(y_variable), ymax = Upper), 
                  position = position_dodge(width = 0.9), 
                  width = 0.25, color = "black") + 
    scale_x_discrete(x_label, expand = c(0.005, 0), 
                     guide = guide_axis(angle = -45)) +
    scale_y_continuous(y_label, expand = c(0, 0),
                       limits = c(0, ceiling(max_upper * 1.05)),
                       breaks = pretty(0:2 * ceiling(max(ungroup(df) %>% 
                                                           dplyr::select(Upper), na.rm = TRUE) / 2))) + 
    ggtitle(title) + 
    BaseForm +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0, vjust = 0)) 
  
  return(plot_list)
}



# ----------------------------
# Function to extract diagnostics
# ----------------------------
extract_diagnostics <- function(model, model_name = "Model") {

  # 1. R-squared (handles NA for conditional R2)
  r2_vals <- performance::r2(model)
  r2_marg <- unname(r2_vals$R2_marginal)
  r2_cond <- if ("R2_conditional" %in% names(r2_vals)) unname(r2_vals$R2_conditional) else NA
  
  # 2. Dispersion parameter
  disp <- performance::check_overdispersion(model)$dispersion_ratio

  # 3. Return a one-row dataframe
  data.frame(
    Model = model_name,
    Num_Observations = nobs(model),
    R2_Marginal = r2_marg,
    R2_Conditional = r2_cond,
    Dispersion_Ratio = disp
  )
}

# ----------------------------
# Example: Run for Historic + Restoration
# ----------------------------

# diag_historic <- extract_diagnostics(NumLive_Historic,  "Historic Model")
# diag_restoration <- extract_diagnostics(NumLive_Restoration, "Restoration Model")
# 
# # Combine into Table A1
# TableA1 <- rbind(diag_historic, diag_restoration)


