
library(lme4)
library(lmerTest) 
library(tidyverse)

suva <- read.csv("Data/SUVA_Slopes.csv")

neon <- read.csv("Data/NEON_Field_Site_Metadata_20251125.csv")


analysis_data <- suva %>%
  left_join(neon, by = c("siteID" = "site_id")) %>%
  mutate(
    mean_annual_temp_C = as.numeric(gsub("°C", "", mean_annual_temperature_C)),
        mean_elevation_m = mean_evelation_m,
        primary_nlcd = as.factor(sapply(strsplit(as.character(dominant_nlcd_classes), "\\|"), `[`, 1))
  ) %>%
  select(siteID, date, Sr, mean_annual_temp_C, mean_annual_precipitation_mm, 
         mean_elevation_m, watershed_size_km2, primary_nlcd) %>%
  na.omit()





# Fit the Linear Mixed-Effects Model
lmm_model <- lmer(Sr ~ mean_annual_temp_C + mean_annual_precipitation_mm + 
                    mean_elevation_m + watershed_size_km2 + primary_nlcd + 
                    (1 | siteID), 
                  data = analysis_data)
summary(lmm_model)

## Better to scale since drivers range are not uniform (watershed area in thousands where temeperature in within shorter range)
lmm_model_scaled <- lmer(Sr ~ scale(mean_annual_temp_C) + 
                           scale(mean_annual_precipitation_mm) + 
                           scale(mean_elevation_m) + 
                           scale(watershed_size_km2) + 
                           primary_nlcd + (1 | siteID), 
                         data = analysis_data)


summary(lmm_model_scaled)




#### Multiple linear regression model
site_level_data <- analysis_data %>%
  group_by(siteID, mean_annual_temp_C, mean_annual_precipitation_mm, 
           mean_elevation_m, watershed_size_km2, primary_nlcd) %>%
  summarize(mean_Sr = mean(Sr, na.rm = TRUE), .groups = 'drop')

lm_model <- lm(mean_Sr ~ mean_annual_temp_C + mean_annual_precipitation_mm + 
                 mean_elevation_m + watershed_size_km2 + primary_nlcd, 
               data = site_level_data)

summary(lm_model)
anova(lm_model)
