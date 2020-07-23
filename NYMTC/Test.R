d1 = data.table::fread("ACS_NYC_CT_part.csv",h=T)
d1$geoid = as.character(d1$geoid)
d2 = data.table::fread("ACS_NYC_NJ_part.csv",h=T)
d2$geoid = as.character(d2$geoid)
d3 = data.table::fread("ACS_NYC_NY_part.csv",h=T)
d3$geoid = as.character(d3$geoid)

d4 = rbind(d1, d2, d3)

d4$GEOID1 = as.character(d4$geoid)
d5 = d4 %>% as_tibble() %>% select(-c(geoid))
data.table::fwrite(d5, "nymtc_census.csv")


mode_df = f0 %>% select(GEOID, mode_pop, da_pop, hov_pop, pt_pop, bus_pop,strcar_pop,
                        subway_pop, rail_pop, boat_pop, taxi_pop, motor_pop, bike_pop, walk_pop,
                        other_pop, wfh_pop)

mode_df = mode_df %>% mutate(da_p = da_pop / mode_pop,
hov_p = hov_pop / mode_pop,
bus_p = (bus_pop + boat_pop) / mode_pop,
subway_p = (strcar_pop + subway_pop + rail_pop) / mode_pop,
taxi_p = taxi_pop / mode_pop,
bike_p = bike_pop / mode_pop,
wlk_p = walk_pop / mode_pop,
wfh_p = wfh_pop / mode_pop,
othr_p = (other_pop + motor_pop) / mode_pop)


### SLD Data ###
us_bg = sf::st_read(dsn = paste0(getwd(), "/epa_sld_2012.geojson"))
us_bg = us_bg %>% mutate(cnty_fips = substr(GEOID10, 1, 5))

bg_shp = sf::st_read(dsn = "NYMTC_BG.geojson")
bg_shp = bg_shp %>% mutate(cnty_fips = substr(GEOID, 1, 5))
bg_shp$GEOID = as.character(bg_shp$GEOID)
dfw_cnty = unique(bg_shp$cnty_fips)

us_bg = us_bg %>% mutate(DFW = as.character(cnty_fips %in% dfw_cnty))

dfw_bg = us_bg %>% filter(DFW == TRUE)

dfw_sld = dfw_bg %>% as_tibble() %>% select(GEOID10, D1A, D1B, D1C, D1D,D2C_TRIPEQ,
                                            D3a,D3aao,D3amm,D3apo,D4a,D4b025,D4b050,D4c,D4d,D5cri,D5dri,
                                            D5ar,D5ae,D5br,D5be)

dfw_sld[dfw_sld == -99999] <- 0
