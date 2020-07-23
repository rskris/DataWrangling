#################################################################################
rm(list = ls())
options(scipen = '10')
list.of.packages <-
  c("tidyverse",
    "data.table",
    "sf",
    "tmap",
    "tmaptools")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
lapply(list.of.packages, require, character = TRUE)
#################################################################################
#install_github("f1kidd/fmlogit")
path2file <-
  "/Users/3kr/Dropbox (ORNL)/Projects/COVID-19 NYC/"
setwd(path2file)


f0 = data.table::fread("nymtc_census.csv", h = T)
f0$GEOID = as.character(f0$GEOID1)
f0[f0 == -666666666] <- 0
f0 = f0 %>% mutate(n_char = nchar(f0$GEOID))
f0 = f0 %>% mutate(GEOID_NEW = ifelse(n_char == 11, paste0("0", GEOID), GEOID))
f0 = f0 %>% select(-c(GEOID, GEOID1, n_char)) %>% rename(GEOID = GEOID_NEW)
colnames_df = data.table::fread("colnames_df.csv", h = T)
colnames(f0) = names(colnames_df)
f0 = f0 %>% rename(GEOID = geoid)

f1 = f0 %>% mutate(
  da_p = da_pop / mode_pop,
  hov_p = hov_pop / mode_pop,
  bus_p = (bus_pop + boat_pop) / mode_pop,
  subway_p = (strcar_pop + subway_pop + rail_pop) / mode_pop,
  bike_p = bike_pop / mode_pop,
  wlk_p = walk_pop / mode_pop,
  wfh_p = wfh_pop / mode_pop,
  othr_p = (other_pop + motor_pop + taxi_pop) / mode_pop
) ## Combining the mode shares into groups for final estimation.

f1 = f1 %>% select(GEOID, da_p, hov_p, bus_p, subway_p, bike_p, wlk_p, wfh_p, othr_p) %>%
  drop_na()

### NYMTC Regional Commute Mode Shares ### 
mode_df = data.frame(
  Mode = c("DA", "HOV", "BUS", "SUBWY", "BIKE", "WALK", "WFH", "OTHRT"),
  ModeShare = c(
    mean(f1$da_p),
    mean(f1$hov_p),
    mean(f1$bus_p),
    mean(f1$subway_p),
    mean(f1$bike_p),
    mean(f1$wlk_p),
    mean(f1$wfh_p),
    mean(f1$othr_p)
  )
)
mode_df$ModeShare = round(mode_df$ModeShare * 100, 2)
print(mode_df)

bg_shp = sf::st_read(dsn = "NYMTC_BG.geojson")
bg_shp = bg_shp %>% mutate(cnty_fips = substr(GEOID, 1, 5))
bg_shp$GEOID = as.character(bg_shp$GEOID)

bg_shp = bg_shp %>% left_join(f1, by = c("GEOID"))

### Mapping DA Mode Share ###
nymtc_shp = tm_shape(bg_shp) + tm_polygons(
  col = "da_p",
  style = "jenks",
  palette = "RdYlGn",
  title.col = "DA Mode Share"
)
tmap_mode("view")
print(nymtc_shp)

### Mapping Subway Mode Share ###
nymtc_shp = tm_shape(bg_shp) + tm_polygons(
  col = "subway_p",
  style = "jenks",
  palette = "RdYlGn",
  title.col = "DA Mode Share"
)
tmap_mode("view")
print(nymtc_shp)
