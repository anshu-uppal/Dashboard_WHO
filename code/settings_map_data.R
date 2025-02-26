pacman::p_load(
        htmltools,
        sf,
        rnaturalearth,
        fuzzyjoin
)

world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
world_filtered <- world |> filter(name_long != "Antarctica") |> 
        relocate(admin, name_long) |> 
        mutate(
                admin = case_match(
                        admin,
                        "Bolivia" ~ "Bolivia (Plurinational State of)",
                        "Republic of the Congo" ~ "Congo",
                        "North Korea" ~ "Democratic People's Republic of Korea",
                        "eSwatini" ~ "Eswatini",
                        "Iran" ~ "Iran (Islamic Republic of)",
                        "Laos" ~ "Lao People's Democratic Republic",
                        "Federated States of Micronesia" ~ "Micronesia (Federated States of)",
                        "Netherlands" ~ "Netherlands (Kingdom of the)",
                        "Moldova" ~ "Republic of Moldova",
                        "São Tomé and Principe" ~ "Sao Tome and Principe",
                        "Syria" ~ "Syrian Arab Republic",
                        "United Kingdom" ~ "The United Kingdom",
                        # ~ "Tokelau",
                        "Turkey" ~ "Türkiye",
                        "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
                        "Vietnam" ~ "Viet Nam",
                        "Palestine" ~ "occupied Palestinian territory",
                        .default = admin
                ))

settings <- tibble(setting = unique(rep_comb$setting)) |> droplevels()

world_settings <- 
        fuzzyjoin::stringdist_left_join(
                settings, world_filtered,
                by = c(setting = "admin"),
                distance_col = "dist"
        ) |> 
        group_by(setting) |>
        slice_min(order_by=dist, n=1) |>
        relocate(dist, .after = admin) |> 
        ungroup()

world_settings_b <- world_settings |> 
        filter(is.na(dist) | dist != 0) |> 
        select(setting) |> 
        fuzzyjoin::stringdist_left_join(
                world_filtered,
                by = c(setting = "name_long"),
                distance_col = "dist"
        ) |> 
        group_by(setting) |>
        slice_min(order_by=dist, n=1) |>
        relocate(dist, .after = admin) |> 
        ungroup()

settings_remaining <- world_settings_b |> 
        filter(is.na(dist) | dist != 0) |> 
        select(setting) |> 
        distinct()

world_settings_matched <- 
        world_settings |> 
        filter(dist ==0) |> 
        bind_rows(world_settings_b |> filter(dist == 0)) |> 
        st_as_sf()


world_c <- anti_join(world_filtered, 
                     world_settings_matched |> select(admin))

saveRDS(world_settings_matched, here::here("data", "world_settings_matched.rds"))
# leaflet(world_settings_matched) %>%
#         addPolygons(
#                 fillColor = "transparent",
#                 color = "black",  # Outline color
#                 weight = 1,
#                 label = ~htmltools::htmlEscape(admin)
#         ) 