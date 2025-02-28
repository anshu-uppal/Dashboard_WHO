# Health inequality data repository
# https://www.who.int/data/inequality-monitor/data

pacman::p_load(
        here,
        tidyverse
)

# # Convert downloaded xlsx files to RDS format for easier downstream use ####
# # Uncomment these below lines if not already generated:
# rep_imm <- openxlsx::read.xlsx(here::here("data", "rep_imm", "data.xlsx"))
# saveRDS(rep_imm, here::here("data", "rep_imm", "rep_imm.rds"))
# 
# rep_tb <- openxlsx::read.xlsx(here::here("data", "rep_tb", "data.xlsx"))
# saveRDS(rep_tb, here::here("data", "rep_tb", "rep_tb.rds"))
# 
# rep_unaids_hiv <- openxlsx::read.xlsx(here::here("data", "rep_unaids_hiv", "data.xlsx"))
# saveRDS(rep_unaids_hiv, here::here("data", "rep_unaids_hiv", "rep_unaids_hiv.rds"))


# Read in data ####
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

# Save combined dataset
saveRDS(rep_comb, here::here("data", "combined_dataset.rds"))
