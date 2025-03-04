# Health inequality data repository
# https://www.who.int/data/inequality-monitor/data

pacman::p_load(
        here,
        tidyverse,
        sf,
        rnaturalearth
)

# # Convert downloaded xlsx files to RDS format for easier downstream use ####
# # Uncomment lines below and run if not already generated:
# rep_imm <- openxlsx::read.xlsx(here::here("data", "rep_imm", "data.xlsx"))
# saveRDS(rep_imm, here::here("data", "rep_imm", "rep_imm.rds"))
# 
# rep_tb <- openxlsx::read.xlsx(here::here("data", "rep_tb", "data.xlsx"))
# saveRDS(rep_tb, here::here("data", "rep_tb", "rep_tb.rds"))
# 
# rep_unaids_hiv <- openxlsx::read.xlsx(here::here("data", "rep_unaids_hiv", "data.xlsx"))
# saveRDS(rep_unaids_hiv, here::here("data", "rep_unaids_hiv", "rep_unaids_hiv.rds"))


# Read in datasets ####
rep_imm <- readRDS(here::here("data", "rep_imm", "rep_imm.rds")) |> 
        mutate(dataset_id_name = "Childhood immunization") |> 
        select(-regcode)

rep_tb <- readRDS(here::here("data", "rep_tb", "rep_tb.rds")) |> 
        mutate(dataset_id_name = "Tuberculosis indicators")

rep_unaids_hiv <- readRDS(here::here("data", "rep_unaids_hiv", "rep_unaids_hiv.rds")) |> 
        mutate(dataset_id_name = "HIV epidemiological estimates") |> 
        mutate(flag = as.character(flag))

# Combine datasets ####
rep_comb <- 
        rep_imm |> 
        bind_rows(rep_tb) |> 
        bind_rows(rep_unaids_hiv) |>
        # Remove rows with no estimate data
        filter(!is.na(estimate)) |> 
        mutate(
                year = sprintf("%1.0f",date),
                date = ymd(paste0(date, "-01-01"))) |> # convert to date format
        # Make a column with the full estimate (estimate [95% CI])
        mutate(full_estimate = case_when(
                estimate<0.01 ~ paste0(sprintf("%1.4f",estimate), " [", 
                                       sprintf("%1.4f",ci_lb), "-",
                                       sprintf("%1.4f",ci_ub), "]"),
                estimate<0.1 ~ paste0(sprintf("%1.3f",estimate), " [", 
                                      sprintf("%1.3f",ci_lb), "-",
                                      sprintf("%1.3f",ci_ub), "]"),
                estimate<2 ~ paste0(sprintf("%1.2f",estimate), " [", 
                                    sprintf("%1.2f",ci_lb), "-",
                                    sprintf("%1.2f",ci_ub), "]"),
                .default = paste0(sprintf("%1.1f",estimate), " [", 
                                  sprintf("%1.1f",ci_lb), "-",
                                  sprintf("%1.1f",ci_ub), "]"))
        ) |>
        droplevels()

# Join with world map data ####
world_final <- 
        # Get the map data
        rnaturalearth::ne_countries(scale = "large", returnclass = "sf") |> 
        filter(name_long != "Antarctica") |> 
        select(admin, name_long, 
               iso_a3,iso_a3_eh, # Country codes for matching with setting data
               formal_en, 
               pop_est:income_grp) |> 
        # Merge the map data with our settings data
        left_join(
                rep_comb |> select(setting, iso3) |> distinct(), 
                by = join_by(iso_a3_eh == iso3)
        ) |> 
        # Add a variable to use the "admin" name when "setting" is NA
        mutate(admin2 = case_when(
                is.na(setting) ~ admin,
                .default = setting
        )) |> 
        relocate(setting, admin, admin2)

# Save data ####
# Save combined dataset
saveRDS(rep_comb, here::here("data", "combined_dataset.rds"))
# Save the mapping data
saveRDS(world_final, here::here("data", "world_settings_matched.rds"))
