# Health inequality data repository
# https://www.who.int/data/inequality-monitor/data

pacman::p_load(
        here,
        tidyverse
)

# Convert downloaded xlsx files to RDS format for easier downstream use
rep_imm <- openxlsx::read.xlsx(here::here("data", "rep_imm", "data.xlsx"))
saveRDS(rep_imm, here::here("data", "rep_imm", "rep_imm.rds"))