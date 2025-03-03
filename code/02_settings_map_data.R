pacman::p_load(
        here,
        tidyverse,
        htmltools,
        sf,
        rnaturalearth,
        fuzzyjoin
)

# Read in combined dataset
rep_comb <- readRDS(here::here("data", "combined_dataset.rds"))

# Get world map data
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
world_filtered <- world |> filter(name_long != "Antarctica") |> 
        relocate(admin, name_long, iso_a3, iso_a3_eh) |>
        select(admin, name_long, iso_a3,iso_a3_eh, formal_en, 
               pop_est:income_grp)

settings <- tibble(setting = unique(rep_comb$setting)) |> droplevels()
settings <- rep_comb |> select(setting, iso3) |> distinct()

world_final <- world_filtered |> 
        left_join(settings, by = join_by(iso_a3_eh == iso3)) |> 
        relocate(setting)

saveRDS(world_final, here::here("data", "world_settings_matched.rds"))