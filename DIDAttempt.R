# DID Attempt 3-DEC 2024

rm(list = ls())

# Load necessary libraries
library(broom)
library(changepoint)
library(countrycode)
library(data.table)
library(dplyr)
library(did)
library(ggplot2)
library(kableExtra)
library(knitr)
library(lubridate)
library(mice)
library(patchwork)
library(plm)
library(readr)
library(readxl)
library(rnaturalearth)
library(sf)
library(stringr)
library(tidyr)
library(lmtest)

# Read data
pdata <- read_csv("pdata_export.csv")

# Create treatment variable and add iso3n
pdata <- pdata %>%
  filter(adaptation_law_stock != 0) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  slice(1) %>%
  rename(law_treat = year) %>%
  select(iso3c, law_treat) %>%
  right_join(pdata, by = "iso3c") %>%
  mutate(iso3n = countrycode(iso3c, origin = "iso3c", destination = "iso3n"))

# Define the list of data frames
data_frames <- list(
  pdata = pdata,
  pdata_seg1 = pdata %>% filter(year >= 1960 & year <= 1998 & law_treat >= 1960 & law_treat <= 1998),  # Segment 1: 1960-1998, treatment between 1960-1998
  pdata_seg2 = pdata %>% filter(year >= 1996 & year <= 2007 & law_treat >= 1996 & law_treat <= 2007),  # Segment 2: 1999-2006, treatment between 1999-2006
  pdata_seg3 = pdata %>% filter(year >= 2007 & year <= 2014 & law_treat >= 2007 & law_treat <= 2014),  # Segment 3: 2007-2014, treatment between 2007-2014
  pdata_seg4 = pdata %>% filter(year >= 2015 & law_treat >= 2015)   # Segment 4: 2015+, treatment in or after 2015
)


# Define a function to process each data frame
process_and_plot_did <- function(df, df_name) {
  
  # Set seed for reproducibility
  set.seed(1995)
  
  # Conditional xformla for pdata_seg2 and pdata_seg3
  xformla_value <- if(df_name %in% c("pdata_seg2", "pdata_seg3")) {
    ~ ROL_perc + ln_gdp + ln_gdp_square + percent_gdp_services + imports_pGDP + auton + clim_vulnerability
  } else {
    NULL  # No xformla for pdata, pdata_seg1, and pdata_seg4
  }
  
  # RUN DID analysis
  did_result <- did::att_gt(
    yname = "readiness",                     # Dependent variable
    tname = "year",                           # Time variable
    idname = "iso3n",                        # Country ID
    gname = "law_treat",                     # Treatment variable
    xformla = xformla_value,                 # Conditional xformla
    control_group = "notyettreated",         # Control group specification
    bstrap = TRUE,                           # Enable bootstrapping
    data = df,                               # Dataset
    est_method = "dr"                        # Doubly robust estimation
  )
  
  # Aggregate the event study results
  did_event <- aggte(did_result, type = "dynamic", na.rm = TRUE)
  
  # Print results and plot
  print(did_event)
  
  # Set the plot title based on df_name
  plot_title <- case_when(
    df_name == "pdata" ~ "DID Event Plot: Climate Readiness (All Years)",
    df_name == "pdata_seg1" ~ "DID Event Plot: Climate Readiness (Pre-1998)",
    df_name == "pdata_seg2" ~ "DID Event Plot: Climate Readiness (1999-2006)",
    df_name == "pdata_seg3" ~ "DID Event Plot: Climate Readiness (2007-2015)",
    df_name == "pdata_seg4" ~ "DID Event Plot: Climate Readiness (Post-2016)",
    TRUE ~ "DID Event Plot"
  )
  
  # Create the plot
  plot <- ggdid(did_event) + 
    ggtitle(plot_title) +  # Set dynamic plot title
    theme(
      text = element_text(family = "Times New Roman"),  # Set Times New Roman font
      axis.title.x = element_text(size = 10),          # Smaller x-axis label
      axis.title.y = element_text(size = 10),          # Smaller y-axis label
      axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1),  # Slanted, black, smaller x-axis tick labels
      axis.text.y = element_text(size = 9, color = "black"), # Smaller, black y-axis tick labels
      plot.title = element_text(hjust = 0.5, size = 14, color = "black"), # Center and resize plot title
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank()   # Remove minor grid lines
    ) +
    scale_color_manual(
      values = c("#FF7F7F", "purple"),            # Red and purple colors
      labels = c("Pre-treatment", "Post-treatment")  # Custom labels
    ) +  
    guides(color = guide_legend(title = "")) +  # Add a legend title
    labs(
      x = "Time to Treatment (Years)",  # x-axis label
      y = "ATTs"                       # y-axis label
    )
  
  print(plot)
}

# Loop through each data frame in the list and process
for (df_name in names(data_frames)) {
  process_and_plot_did(data_frames[[df_name]], df_name)
}
