# 3-DEC 2024 
# SegReg Models 
## > Plus financial crisis variable
## > Plus Staboility variable: https://databank.worldbank.org/source/worldwide-governance-indicators/Series/PV.EST#


rm(list = ls())

################
## LIBRARIES ##
################

library(broom)
library(changepoint)
library(countrycode)
library(data.table)
library(dplyr)
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
library(vdemdata)
library(VIM)
library(zoo)


pdata <- read_csv("pdata_export.csv")

## ADD RECESSION VAIRABLES
pdata <- pdata %>%
  mutate(financial_recession = ifelse(year >= 2007 & year <= 2009, 1, 0))

# Load the V-Party dataset: https://www.v-dem.net/documents/6/vparty_codebook_v2.pdf
vparty <- vdemdata::vparty %>%
  mutate(iso3c = countrycode(country_id, origin = "vdem", destination = "iso3c")) %>%
  filter(!is.na(iso3c))  # Only keep rows where iso3c is not missing
# Calculate both average and maximum populism scores for each country-year
country_year_populism <- vparty %>%
  group_by(iso3c, year) %>%
  summarize(
    avg_populism = mean(v2xpa_popul, na.rm = TRUE),
    max_populism = ifelse(any(!is.na(v2xpa_popul)), max(v2xpa_popul, na.rm = TRUE), NA),
    .groups = 'drop'  # to ungroup the data after summarization
  )
pdata <- pdata %>%
  left_join(country_year_populism, by = c("iso3c", "year"))

## ADD STABILITY VARIABLE

stability <- read_excel("stability.xlsx") %>%
  mutate(iso3c = countrycode(wbcode, origin = "wb", destination = "iso3c")) %>%
  filter(!is.na(iso3c))

# Merge stability onto pdata by iso3c and year
pdata <- pdata %>%
  left_join(stability %>% select(-CountryName, -wbcode), by = c("iso3c", "year"))

# Fill missing values for both polstability_estimate and polstability_percentile within each iso3c group
pdata <- pdata %>%
  group_by(iso3c) %>%
  mutate(
    # Forward fill polstability_estimate
    polstability_estimate = zoo::na.locf(polstability_estimate, na.rm = FALSE),
    # Backward fill polstability_estimate
    polstability_estimate = zoo::na.locf(polstability_estimate, fromLast = TRUE, na.rm = FALSE),
    
    # Forward fill polstability_percentile
    polstability_percentile = zoo::na.locf(polstability_percentile, na.rm = FALSE),
    # Backward fill polstability_percentile
    polstability_percentile = zoo::na.locf(polstability_percentile, fromLast = TRUE, na.rm = FALSE),
    
    # Forward fill avg_populism
    avg_populism = zoo::na.locf(avg_populism, na.rm = FALSE),
    # Backward fill avg_populism
    avg_populism = zoo::na.locf(avg_populism, fromLast = TRUE, na.rm = FALSE),
    
    # Forward fill max_populism
    max_populism = zoo::na.locf(max_populism, na.rm = FALSE),
    # Backward fill max_populism
    max_populism = zoo::na.locf(max_populism, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  ungroup()


# Segment the data into four groups based on year ranges
pdata_seg1 <- subset(pdata, year <= 1996)
pdata_seg2 <- subset(pdata, year > 1996 & year <= 2006)
pdata_seg3 <- subset(pdata, year > 2006 & year <= 2015)
pdata_seg4 <- subset(pdata, year > 2015)

print(colnames(pdata))


# Function to create models for each segment and lag
create_models <- function(segment_data, dv, max_lag) {
  models <- list()
  for (lag in c(1, 3, 5, 7)) {  # Adjusted for lags 1, 3, 5, and 7
    model_name <- paste0("Segment ", segment_data$segment, " | ", dv, " (Lag ", lag, ")")
    model_formula <- reformulate(c("adaptation_law_stock", "ROL_perc", "ln_gdp", "ln_gdp_square", "percent_gdp_services", "imports_pGDP", "auton", "clim_vulnerability", "avg_populism", "financial_recession"), response = dv)
    model <- plm(formula = model_formula, data = segment_data, index = c("iso3c", "year"), effect = "twoways", model = "within", vcov = "HC1")
    models[[model_name]] <- model
  }
  return(models)
}
# Create models for each segment and lag
max_lag <- 3
segments <- list(pdata, pdata_seg1, pdata_seg2, pdata_seg3, pdata_seg4)
segment_names <- c("Full Data", "Segment 1", "Segment 2", "Segment 3", "Segment 4")
dependent_variables <- c("lag1_readiness", "lag3_readiness", "lag5_readiness", "lag7_readiness")  # Updated dependent variables

all_models <- list()
for (i in 1:length(segments)) {
  segment_data <- segments[[i]]
  for (dv in dependent_variables) {
    models <- create_models(segment_data, dv, max_lag)
    all_models <- c(all_models, list(models))
  }
}

# Print summaries of the models
for (i in 1:length(all_models)) {
  segment_models <- all_models[[i]]
  printed <- FALSE
  for (model_name in names(segment_models)) {
    model <- segment_models[[model_name]]
    if (!is.null(model) && !printed) {
      print(summary(model))
      tidy_results <- tidy(model) %>% mutate_at(vars(estimate, std.error), ~sprintf("%.4f", .x)) %>% mutate(significance = ifelse(p.value < 0.001, "***", ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", ""))))
      tidy_results$coef_with_significance <- paste0(tidy_results$estimate, " (", tidy_results$std.error, ") ", tidy_results$significance)
      table_data <- tidy_results[, c("term", "coef_with_significance")]
      names(table_data) <- c("Variable", "Coefficient Estimate (SE)")
      cat("## Model Summary\n")
      print(kable(table_data, format = "markdown", align = "c"))
      cat("\n")
      printed <- TRUE
    }
  }
}


##### LONG TERM EFFECTS ONLY #####

# Function to create models for each segment and lag
create_models <- function(segment_data, dv, max_lag) {
  models <- list()
  for (lag in c(5, 7)) {  # Adjusted for lags 5 and 7
    model_name <- paste0("Segment ", segment_data$segment, " | ", dv, " (Lag ", lag, ")")
    model_formula <- reformulate(c("adaptation_law_stock", "ROL_perc", "ln_gdp", "ln_gdp_square", "percent_gdp_services", "imports_pGDP", "auton", "clim_vulnerability", "avg_populism", "financial_recession"), response = dv)
    model <- plm(formula = model_formula, data = segment_data, index = c("iso3c", "year"), effect = "twoways", model = "within", vcov = "HC1")
    models[[model_name]] <- model
  }
  return(models)
}

# Create models for each segment and lag
max_lag <- 7  # Adjusted for the maximum lag
segments <- list(pdata, pdata_seg1, pdata_seg2, pdata_seg3, pdata_seg4)
segment_names <- c("Full Data", "Segment 1", "Segment 2", "Segment 3", "Segment 4")
dependent_variables <- c("lag1_readiness", "lag7_readiness")  # Updated dependent variables

all_models <- list()
for (i in 1:length(segments)) {
  segment_data <- segments[[i]]
  for (dv in dependent_variables) {
    models <- create_models(segment_data, dv, max_lag)
    all_models <- c(all_models, list(models))
  }
}

# Print summaries of the models
for (i in 1:length(all_models)) {
  segment_models <- all_models[[i]]
  printed <- FALSE
  for (model_name in names(segment_models)) {
    model <- segment_models[[model_name]]
    if (!is.null(model) && !printed) {
      print(summary(model))
      tidy_results <- tidy(model) %>% mutate_at(vars(estimate, std.error), ~sprintf("%.4f", .x)) %>% mutate(significance = ifelse(p.value < 0.001, "***", ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", ""))))
      tidy_results$coef_with_significance <- paste0(tidy_results$estimate, " (", tidy_results$std.error, ") ", tidy_results$significance)
      table_data <- tidy_results[, c("term", "coef_with_significance")]
      names(table_data) <- c("Variable", "Coefficient Estimate (SE)")
      cat("## Model Summary\n")
      print(kable(table_data, format = "markdown", align = "c"))
      cat("\n")
      printed <- TRUE
    }
  }
}



## CHECKING FINANCIAL CRISIS
# Define the formula
model_formula <- lag1_readiness ~ adaptation_law_stock + ROL_perc + ln_gdp + 
  ln_gdp_square + percent_gdp_services + imports_pGDP + 
  auton + clim_vulnerability + max_populism + 
  financial_recession

# Run the random effects model with plm
model_plm_random <- plm(formula = model_formula, 
                        data = pdata, # and with pdata (full) - both show that the reseccion decreases resilience
                        index = c("iso3c", "year"),  # specify the panel structure (country, year)
                        model = "random",             # random effects
                        vcov = "HC1")                 # heteroscedasticity-consistent standard errors

# Print the summary of the model
summary(model_plm_random)


## FINDINGS 
# Recession = less likely readiness in full & recession samples
# Stability variable showed opposite results (e.g. more unstable, more ready)
# Same with populism variable from VDEM


avg_populism_by_year <- pdata %>%
  group_by(year) %>%
  summarize(avg_populism = mean(avg_populism, na.rm = TRUE))

# Create the plot
plot <- ggplot(avg_populism_by_year, aes(x = year, y = avg_populism)) +
  geom_line(color = "purple") +
  geom_point(color = "purple") +
  labs(title = "Average Levels of Populism Over Time",
       x = "Year",
       y = "Average Populism") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text())

# Display the plot
print(plot)