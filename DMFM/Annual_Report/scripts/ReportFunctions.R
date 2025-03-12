# Create all Functions necessary for Report

### Filter Functions ###
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
      grepl("SBM", StationName) & TimeSinceCultched > 0 ~ "NFWF-2021",
      TRUE ~ "Historic Uncultched")) 
}
###


### Survey Functions ###
summarise_ALL <- function(data) {
  summarised_data <- data %>%
    summarise(NumLiveMean = mean(NumLive, na.rm = TRUE),
              NumLiveSD = sd(NumLive, na.rm = TRUE),
              TotalWeightMean = mean(TotalWeight, na.rm = TRUE),
              TotalWeightSD = sd(TotalWeight, na.rm = TRUE)) %>%
    mutate(OysterDensityMean = NumLiveMean * 4,
           OysterDensitySD = NumLiveSD * 4,
           WeightDensityMean = TotalWeightMean * 4,
           WeightDensitySD = TotalWeightSD * 4)
  return(summarised_data)
}

summarise_ClassicSizeClass <- function(data) {
  summarised_data <- data %>%
    summarise(Num_Spat_1_to_30_Mean = mean(Num_Spat_1_to_30, na.rm = TRUE),
              Num_Seed_31_to_75_Mean = mean(Num_Seed_31_to_75, na.rm = TRUE),
              Num_Legal_76_plus_Mean = mean(Num_Legal_76_plus, na.rm = TRUE)) %>%
    mutate(Density_Spat_1_to_30_Mean = Num_Spat_1_to_30_Mean * 4,
           Density_Seed_31_to_75_Mean = Num_Seed_31_to_75_Mean * 4,
           Density_Legal_76_plus_Mean = Num_Legal_76_plus_Mean * 4,
           SumDensityMean = Density_Spat_1_to_30_Mean + Density_Seed_31_to_75_Mean + Density_Legal_76_plus_Mean)
  return(summarised_data)
}

summarise_LegalONLY <- function(data) {
  summarised_data <- data %>%
    summarise(Num_Legal_76_plus_Mean = mean(Num_Legal_76_plus, na.rm = TRUE),
              Num_Legal_76_plus_SD = sd(Num_Legal_76_plus, na.rm = TRUE)) %>%
    mutate(Density_Legal_76_plus_Mean = Num_Legal_76_plus_Mean * 4,
           Density_Legal_76_plus_SD = Num_Legal_76_plus_SD * 4) %>%
    mutate(BagsPerAcre = (Density_Legal_76_plus_Mean * 4047) / 225,
           BagsPerAcreSD = (Density_Legal_76_plus_SD * 4047) / 225) %>%
    mutate(MaxBags = BagsPerAcre + BagsPerAcreSD,
           MinBags = BagsPerAcre - BagsPerAcreSD)
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
