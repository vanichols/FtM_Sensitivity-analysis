library(tidyverse)
library(stringr)

######################################
# Import files
######################################
# sensitivity analysis outputs -------
num <- str_pad(seq(1, 16, 1), 2, pad = "0")

# irrigated
reg_list_1 <- list()

for (i in 1:length(num)) {
  reg_list_1[[i]] <- read_csv(paste0("raw_data/results_06_14_2019/region_", num[i], "_sensitivity_analysis.csv"))
  reg_list_1[[i]]$region <- i
}

sa_output_1 <- bind_rows(reg_list_1)
sa_output_1$irrigation_status <- "Irrigated"

# rainfed
reg_list_2 <- list()

for (i in 1:length(num)) {
  reg_list_2[[i]] <- read_csv(paste0("raw_data/results_06_21_2019_rainfed/region_", num[i], "_sensitivity_analysis.csv"))
  reg_list_2[[i]]$region <- i
}

sa_output_2 <- bind_rows(reg_list_2)
sa_output_2$irrigation_status <- "Rainfed"

# combine
sa_output <- bind_rows(sa_output_1, sa_output_2)





# reference tables -------
# ref_scenarios <- read_csv("raw_data/ref_table_scenarios.csv")
# ref_region_names <- read_excel("raw_data/FTM Region Descriptions.xlsx")
scenario_lookup <- read_csv("raw_data/scenario_lookup.csv")
region_lookup <- read_csv("raw_data/region_lookup.csv")
metric_lookup <- read_csv("raw_data/metric_lookup.csv")

######################################
# Clean dataset
######################################
# Cleaning variable names
sa_output_2 <- sa_output %>% 
  setNames(make.names(names(.), unique = TRUE)) %>% 
  select_all(tolower) %>% 
  select_all(~str_replace_all(., "\\.", "_")) %>% 
  select_all(~str_replace_all(., "___", "_"))

# Cleaning scenario names
sa_output_3 <- sa_output_2 %>% 
  mutate(change_name = str_replace_all(change_name, "__","_")) %>% 
  mutate(change_name = str_replace_all(change_name, "__","_")) %>% 
  mutate(change_name = str_replace_all(change_name, "-","_")) %>% 
  mutate(change_name = str_replace_all(change_name, ">","more_than_")) %>%
  mutate(change_name = str_replace_all(change_name, "<=","less_than_")) %>% # kathy added
  mutate(change_name = str_replace_all(change_name, "<","less_than_")) %>% 
  mutate(change_name = str_replace_all(change_name, "\\.","_"))

# Adding tillage type
sa_output_4 <- sa_output_3 %>%
  mutate(lmod_template_original = lmod_template) %>% 
  separate(lmod_template, c("region_discard", "crop_discard", "tillage_type", "cover_discard"), sep = " - ") %>% 
  select(-region_discard, -crop_discard, -cover_discard)

# Edit base scenario names
sa_output_5 <- sa_output_4 %>% 
  mutate(base_scenario = case_when(tillage_type == "Conventional Tillage" & cover_crop == F ~ "Conventional, No CC",
                                   tillage_type == "Conventional Tillage" & cover_crop == T ~ "Conventional, With CC",
                                   tillage_type == "Reduced Tillage" & cover_crop == F ~ "Reduced, No CC",
                                   tillage_type == "Reduced Tillage" & cover_crop == T ~ "Reduced, With CC",
                                   tillage_type == "No till" & cover_crop == F ~ "No Till, No CC",
                                   tillage_type == "No till" & cover_crop == T ~ "No Till, With CC")) %>% 
  mutate(base_scenario = factor(base_scenario, levels = c("Conventional, No CC", "Conventional, With CC",
                                                          "Reduced, No CC", "Reduced, With CC",
                                                          "No Till, No CC", "No Till, With CC")))

# Adding scenario reference values
sa_output_6 <- sa_output_5 %>% 
  left_join(scenario_lookup, by = "change_name") %>% 
  left_join(region_lookup, by = "region") 

# Some variables need to be made numeric
sa_output_7 <- sa_output_6 %>%
  mutate(estimated_non_irrigated_yield = as.numeric(estimated_non_irrigated_yield),
         irrigation_water_use_fieldprint = as.numeric(irrigation_water_use_fieldprint))

# Finalized and save 
d01_sa_output <- sa_output_7 %>% 
  mutate(crop = ifelse(crop %in% c("Wheat durum", "Wheat winter", "Wheat spring"), "Wheat", crop))

save(d01_sa_output, file = "r_data/d01_sa_output.rda")

######################################
# Calculate percentage impact
######################################
# create tillage_type and cover_crop groupings
# set REDUCED TILLAGE, NO CC as the base scenario
dat <- d01_sa_output %>% 
  mutate(grouping = ifelse(tillage_type != "Reduced Tillage" & cover_crop == FALSE & grouping == "base", "tillage_type", grouping),
         grouping = ifelse(tillage_type == "Reduced Tillage" & cover_crop != FALSE & grouping == "base", "cover_crop", grouping),
         change_name = ifelse(grouping == "tillage_type", tillage_type, change_name),
         change_name = ifelse(grouping == "cover_crop", cover_crop, change_name),
         base_scenario = ifelse(grouping %in% c("tillage_type", "cover_crop"), "Reduced, No CC", as.character(base_scenario)))

res <- list()
metrics <- metric_lookup$metric

for (i in 1:length(metrics)) {
  
  metric <- metrics[i]
  #metric <- "energy_use_fieldprint"
  
  # get base scenario
  base_scenario <- dat %>% 
    filter(base_scenario == "Reduced, No CC", grouping == "base") %>% 
    select(crop, region, irrigation_status, metric)
  
  # calc pct change
  dat_2 <- dat %>% 
    filter(base_scenario == "Reduced, No CC", grouping != "base") %>% 
    select(crop, region, irrigation_status, grouping, change_name, metric) %>% 
    left_join(base_scenario, by = c("crop", "region", "irrigation_status")) %>% 
    rename(change_val = paste0(metric,".x"),
           base_val = paste0(metric, ".y")) %>% 
    mutate(pct_change = (change_val-base_val)/base_val)
  
  # drop groupings with no change from base
  group_range <- dat_2 %>% 
    group_by(crop, region, irrigation_status, grouping, base_val) %>% 
    summarise(grp_min_val = min(change_val),
              grp_max_val = max(change_val),
              grp_min_pct=min(pct_change), 
              grp_max_pct=max(pct_change)) %>% 
    mutate(grp_min_val=ifelse(grp_min_val>base_val, base_val, grp_min_val),
           grp_max_val=ifelse(grp_max_val<base_val, base_val, grp_max_val),
           grp_min_pct=ifelse(grp_min_pct>0, 0, grp_min_pct),
           grp_max_pct=ifelse(grp_max_pct<0, 0, grp_max_pct),
           grp_range=grp_max_pct-grp_min_pct) %>% 
    select(-base_val)
  
  # merge back
  dat_3 <- dat_2 %>% 
    left_join(group_range, by = c("crop", "region", "irrigation_status", "grouping")) %>% 
    filter(grp_range > 0) %>%
    mutate(metric_name = metric) %>% 
    select(metric_name, everything())
  
  res[[i]] <- dat_3
  
}

d02_pct_impact <- bind_rows(res) %>% 
  rename(metric = metric_name) %>% 
  left_join(scenario_lookup, by = c("change_name", "grouping")) %>% 
  left_join(metric_lookup, by = "metric") %>% 
  left_join(region_lookup, by = "region") %>% 
  mutate(change_name_label = paste(grouping_label, change_name_label, sep = ": "))

save(d02_pct_impact, file = "r_data/d02_pct_impact.rda")

