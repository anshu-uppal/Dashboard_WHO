# Health inequality data repository
# https://www.who.int/data/inequality-monitor/data

pacman::p_load(
        here,
        tidyverse
)

# Convert downloaded xlsx files to RDS format for easier downstream use
rep_imm <- openxlsx::read.xlsx(here::here("data", "rep_imm", "data.xlsx"))
saveRDS(rep_imm, here::here("data", "rep_imm", "rep_imm.rds"))

rep_dtp_sub <- openxlsx::read.xlsx(here::here("data", "rep_dtp_sub", "data.xlsx"))
saveRDS(rep_dtp_sub, here::here("data", "rep_dtp_sub", "rep_dtp_sub.rds"))

rep_unaids_hiv <- openxlsx::read.xlsx(here::here("data", "rep_unaids_hiv", "data.xlsx"))
saveRDS(rep_unaids_hiv, here::here("data", "rep_unaids_hiv", "rep_unaids_hiv.rds"))