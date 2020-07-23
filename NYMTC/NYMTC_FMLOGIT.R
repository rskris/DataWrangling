#################################################################################
rm(list = ls())
options(scipen = '10')
list.of.packages <-
  c(
    "tidyr",
    "ggplot2",
    "data.table",
    "devtools",
    "tibble",
    "dplyr",
    "sf"
  )
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

# multiResultClass <- function(result1 = NULL, result2 = NULL)
# {
#   me <- list(result1 = result1,
#              result2 = result2)
#
#   ## Set the name for the class
#   class(me) <- append(class(me), "multiResultClass")
#   return(me)
# }


# d1 = data.table::fread("ACS_NYC_CT_part.csv", h = T)
# d1$geoid = as.character(d1$geoid)
# d2 = data.table::fread("ACS_NYC_NJ_part.csv", h = T)
# d2$geoid = as.character(d2$geoid)
# d3 = data.table::fread("ACS_NYC_NY_part.csv", h = T)
# d3$geoid = as.character(d3$geoid)
#
# d4 = rbind(d1, d2, d3)
#
# d4$GEOID1 = as.character(d4$geoid)
# d5 = d4 %>% as_tibble() %>% select(-c(geoid))
# data.table::fwrite(d5, "nymtc_census.csv")


#### Census Variables Calculations ####
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
  pop_18yrs = male_5yrs + male_529yrs + male_10214yrs + male_15217yrs
  + fem_5yrs + fem_529yrs + fem_10214yrs + fem_15217yrs,
  elder_65yrs = male_65266yrs + male_67269yrs + male_70274yrs + male_75279yrs +
    male_80284yrs + male_85yrs + fem_65266yrs + fem_67269yrs + fem_70274yrs + fem_75279yrs +
    fem_80284yrs + fem_85yrs
) ## population share of elderly and children <=18 years old.

f1 = f1 %>% mutate(
  white = whitepop / totpop,
  black = blkpop / totpop,
  asian = asianpop / totpop,
  hisp = hisp_pop / totpop,
  hhsize = ifelse(tothh > 0, totpop / tothh, 0)
) ## ethnic make-up of the BG.

f1 = f1 %>% mutate(
  da_p = da_pop / mode_pop,
  hov_p = hov_pop / mode_pop,
  bus_p = (bus_pop + boat_pop) / mode_pop,
  subway_p = (strcar_pop + subway_pop + rail_pop) / mode_pop,
  bike_p = bike_pop / mode_pop,
  wlk_p = walk_pop / mode_pop,
  wfh_p = wfh_pop / mode_pop,
  othr_p = (other_pop + motor_pop + taxi_pop) / mode_pop
) ## Combining the mode shares into groups for final estimation.

f1 = f1 %>% mutate(
  dprt_6am8am = (dprt630am + dprt7am + dprt730am + dprt8am) / dprt_pop,
  dprt_8am10am = (dprt830am + dprt9am + dprt10am) / dprt_pop,
  dprt_10am12pm = (dprt11am + dprt12pm) / dprt_pop,
  dprt_12pm4pm = dprt4pm / dprt_pop,
  dprt_even = dprt12am / dprt_pop,
  dprt_nt = (dprt5am + dprt530am + dprt6am) / dprt_pop
) ## Departure time of workers from home locations.

f1 = f1 %>% mutate(
  tt_15mins = (ttw5 + ttw10 + ttw15) / ttw_pop,
  tt_30mins = (ttw20 + ttw25 + ttw30) / ttw_pop,
  tt_45mins = (ttw35 + ttw40 + ttw45) / ttw_pop,
  tt_60mins = ttw50 / ttw_pop,
  tt_60plus = (ttw90 + ttw_90plus) / ttw_pop
) ## reported travel time of population shares in each group.

f1 = f1 %>% mutate(
  edu_hsaaprof = (edu_aa + edu_hschl + edu_prof) / edu_pop,
  edu_uni = (edu_bs + edu_ms + edu_phd) / edu_pop,
  chld_share = chld_pop / totpop
) ## graduation level of population in BG.


f1 = f1 %>% mutate(
  prime_wrkrs = (male_agri + fem_agri) / workers,
  second_wrkrs = (male_const + male_manuf + fem_const + fem_manuf) /
    workers,
  tert_nonretail = (
    male_whole + male_transpo + male_info + male_fire +
      male_prof + male_eduheal2 + male_arts + male_other +
      fem_whole + fem_transpo + fem_info + fem_fire +
      fem_prof + fem_eduheal2 + fem_arts + fem_other
  ) / workers,
  tert_retail = (male_retail + fem_retail) / workers,
  quat_wrkrs = (male_eduheal1 + male_public + fem_eduheal1 + fem_public) /
    workers
) ## worker occupation classified by primary, secondary, tertiary and quaternary.

f1 = f1 %>% mutate(
  manuf = (male_manuf + fem_manuf) / workers,
  infoW = (male_info + fem_info + male_prof + fem_prof) /
    workers,
  fireW = (male_fire + fem_fire) / workers,
  healthW = (male_eduheal2 + fem_eduheal2) / workers,
  retailW = (male_retail + fem_retail) / workers
) ## NAICS based worker classifications.

f1 = f1 %>% mutate(
  vacant_p = vacant / tothu_o,
  owned = owner_hh / occu_hh,
  rented = rent_hh / occu_hh
) ## home ownership rates in BG.

f1 = f1 %>% mutate(
  veh0 = (own_veh0 + rent_veh0) / veh_oocu,
  veh1 = (own_veh1 + rent_veh1) / veh_oocu,
  veh2 = (own_veh2 + rent_veh2) / veh_oocu,
  veh3 = (own_veh3 + rent_veh3) / veh_oocu,
  veh_3plus = (own_veh4 + own_veh5 + rent_veh4 + rent_veh5) /
    veh_oocu
) ## Vehicle ownership rates in BG.

#### Final list of variables from Census selected in Model Specification ###

df_mod = f1 %>% select(
  GEOID,
  totpop,
  malepop,
  fempop,
  pop_18yrs,
  elder_65yrs,
  medage,
  male_age,
  fem_age,
  hhsize,
  white,
  black,
  asian,
  hisp,
  dprt_6am8am,
  dprt_8am10am,
  dprt_10am12pm,
  dprt_12pm4pm,
  dprt_even,
  tt_15mins,
  tt_30mins,
  tt_45mins,
  tt_60mins,
  edu_hsaaprof,
  edu_uni,
  chld_share,
  prime_wrkrs,
  second_wrkrs,
  tert_nonretail,
  tert_retail,
  quat_wrkrs,
  vacant_p,
  owned,
  rented,
  veh0,
  veh1,
  veh2,
  veh3,
  veh_3plus,
  med_hhinc,
  year_built,
  da_p,
  hov_p,
  bus_p,
  subway_p,
  bike_p,
  wlk_p,
  wfh_p,
  othr_p,
  manuf,
  infoW,
  fireW,
  healthW,
  retailW
)

df_mod = df_mod %>% mutate(
  elder_share = elder_65yrs / totpop,
  male_shr = malepop / totpop,
  log_inc = ifelse(med_hhinc > 0, log(med_hhinc), 0),
  built_1980 = ifelse(year_built < 1980, 1, 0),
  low_inc = ifelse(med_hhinc < 45000, 1, 0),
  middle_inc = ifelse(med_hhinc > 45000 &
                        med_hhinc < 75000, 1, 0),
  high_inc = ifelse(med_hhinc > 75000 &
                      med_hhinc < 125000, 1, 0),
  vhigh_inc = ifelse(med_hhinc > 125000, 1, 0)
)


#### BG-2-BG Distance Data Processing ####
f2 = data.table::fread("NYC_LODES_and_distance.csv", h = T)
f2$O_bg = as.character(f2$O_bg)
f2$D_bg = as.character(f2$D_bg)
f2 = f2 %>% mutate(nchar_O = nchar(f2$O_bg), nchar_D = nchar(f2$D_bg)) %>%
  mutate(
    OBG = ifelse(nchar_O == 11, paste0("0", O_bg), O_bg),
    DBG = ifelse(nchar_D == 11, paste0("0", D_bg), D_bg)
  )
f2 = f2 %>% select(-c(O_bg, D_bg, nchar_O, nchar_D)) %>% arrange(OBG, DBG, S000, distance_miles)


bg_dist = f2 %>% group_by(OBG) %>% summarize(
  avg_dist = mean(distance_miles),
  med_dist = median(distance_miles),
  sd_dist = sd(distance_miles),
  count_bg = n(),
  emp_sum = sum(S000),
  emp_avg = mean(S000))

f3 = df_mod %>% left_join(bg_dist, by = c("GEOID" = "OBG"))

### SLD Data ###
us_bg = sf::st_read(dsn = paste0(getwd(), "/epa_sld_2012.geojson"))
us_bg = us_bg %>% mutate(cnty_fips = substr(GEOID10, 1, 5))

bg_shp = sf::st_read(dsn = "NYMTC_BG.geojson")
bg_shp = bg_shp %>% mutate(cnty_fips = substr(GEOID, 1, 5))
bg_shp$GEOID = as.character(bg_shp$GEOID)
nymtc_cnty = unique(bg_shp$cnty_fips)

us_bg = us_bg %>% mutate(NYMTC = as.character(cnty_fips %in% nymtc_cnty))

nymtc_bg = us_bg %>% filter(NYMTC == TRUE)

nymtc_sld = nymtc_bg %>% as_tibble() %>% select(
  GEOID10,
  D1A,
  D1B,
  D1C,
  D1D,
  D2C_TRIPEQ,
  D3a,
  D3aao,
  D3amm,
  D3apo,
  D4a,
  D4b025,
  D4b050,
  D4c,
  D4d,
  D5cri,
  D5dri,
  D5ar,
  D5ae,
  D5br,
  D5be
) ## refer to the EPA Smart Location Database for data dictionary (https://www.epa.gov/sites/production/files/2014-03/documents/sld_userguide.pdf)

nymtc_sld[nymtc_sld == -99999] <- 0

f3 = f3 %>% left_join(nymtc_sld, by = c("GEOID" = "GEOID10"))
f3[is.na(f3)] <- 0

f3 = f3 %>% mutate(log_dist = ifelse(avg_dist > 0, log(avg_dist), 0)) %>% rename(D5_auto = D5ar,
                                                                                 D5_pt = D5br)
f3 = f3 %>% mutate(D5_access = ifelse(D5_auto > 0, D5_pt / D5_auto, 0),
                   conn_max = count_bg / max(count_bg),
                   emp_con = emp_sum / max(emp_sum),
                   D3_p = ifelse(D3a > 0, D3apo / D3a, 0),
                   D3_mm = ifelse(D3a > 0, D3amm / D3a, 0),
                   D3_auto = ifelse(D3a > 0, D3aao / D3a, 0))

#### Model Estimation ####
require(fmlogit)
set.seed(0)
train_pct = 0.65 ## currently using 65% of the sample for trainig the model.
train_idx = sort(sample(nrow(f3), nrow(f3) * train_pct))
train_df = f3[train_idx, ]
test_df = f3[-train_idx, ]

### Final Specification for NYMTC Region ###
start_time <- Sys.time()

model_df = train_df %>% mutate(const1 = 1)
Y = model_df %>% select(othr_p, da_p, hov_p, bus_p, subway_p, bike_p, wlk_p, wfh_p)
X0 = model_df %>% select(const1)
X1 = model_df %>% select(
  male_shr,
  medage,
  chld_share,
  white,
  black,
  hisp,
  asian,
  dprt_6am8am,
  dprt_8am10am,
  dprt_12pm4pm,
  dprt_even,
  tt_15mins,
  tt_30mins,
  tt_45mins,
  edu_uni,
  manuf,
  infoW,
  fireW,
  healthW,
  retailW,
  owned,
  vacant_p,
  veh0,
  veh1,
  built_1980,
  log_dist,
  conn_max,
  D1B,
  D3apo,
  D3amm,
  D4a,
  D4b025,
  D4c,
  D5_access
)

m0 = fmlogit(Y, X0, MLEmethod = "BHHH")
m1 = fmlogit(Y, X1, MLEmethod = "BHHH")

adj_r = 1 - (m1$likelihood / m0$likelihood)

print(paste0("Adjusted R-Squared is ", round(adj_r, 2)))

end_time <- Sys.time() - start_time
print(end_time)

summary.fmlogit(m1)

#### Prediction Based on Finalized Model ####
Y_test = test_df %>% select(othr_p, da_p, hov_p, bus_p, subway_p, bike_p, wlk_p, wfh_p)
X_test = test_df %>% select(
  male_shr,
  medage,
  chld_share,
  white,
  black,
  hisp,
  asian,
  dprt_6am8am,
  dprt_8am10am,
  dprt_12pm4pm,
  dprt_even,
  tt_15mins,
  tt_30mins,
  tt_45mins,
  edu_uni,
  manuf,
  infoW,
  fireW,
  healthW,
  retailW,
  owned,
  vacant_p,
  veh0,
  veh1,
  built_1980,
  log_dist,
  D1B,
  D3amm,
  D3apo,
  D4a,
  D4b025,
  D4c,
  D5_access
)

f4 = predict.fmlogit(m1, newdata = X_test)

### Prediction data processing ###
Y_test = test_df %>% select(othr_p, da_p, hov_p, bus_p, subway_p, bike_p, wlk_p, wfh_p)
f5 = bind_cols(Y_test, f4)
f5 = f5 %>% drop_na()
CorM = cor(f5, use = "na.or.complete")
CorM_adj = data.frame(
  Modes = c("DA", "HOV", "BUS", "SUBWAY", "BIKE", "WALK", "WFH", "OTHER"),
  CorrCoeff = c(CorM[2, 10], CorM[3, 11], CorM[4, 12], CorM[5, 13],
                CorM[6, 14], CorM[7, 15], CorM[8, 16], CorM[1, 9])
)

### Prediction Diagnostics ###
print(paste0("Correlation Coefficient for Observed and Predicted"))
print(CorM_adj)

## Scatter plots of predicted (X-axis) and observed (Y-axis) ##
par(mfrow = c(2, 2))
plot(f5$da_p1, f5$da_p)
abline(0, 1, col = "red", lwd = 2)
plot(f5$hov_p1, f5$hov_p)
abline(0, 1, col = "red", lwd = 2)
plot(f5$bus_p1, f5$bus_p)
abline(0, 1, col = "red", lwd = 2)
plot(f5$subway_p1, f5$subway_p)
abline(0, 1, col = "red", lwd = 2)

par(mfrow = c(2, 2))
plot(f5$bike_p1, f5$bike_p)
abline(0, 1, col = "red", lwd = 2)
plot(f5$wlk_p1, f5$wlk_p)
abline(0, 1, col = "red", lwd = 2)
plot(f5$wfh_p1, f5$wfh_p)
abline(0, 1, col = "red", lwd = 2)
plot(f5$othr_p1, f5$othr_p)
abline(0, 1, col = "red", lwd = 2)


## Difference of observed & predicted ##
f5 = f5 %>% mutate(
  othr_diff = othr_p - othr_p1,
  da_diff = da_p - da_p1,
  hov_diff = hov_p - hov_p1,
  bus_diff = bus_p - bus_p1,
  subway_diff = subway_p - subway_p1,
  bike_diff = bike_p - bike_p1,
  wlk_diff = wlk_p - wlk_p1,
  wfh_diff = wfh_p - wfh_p1
)

f6 = tidyr::gather(
  f5,
  "da_diff",
  "hov_diff",
  "bus_diff",
  "subway_diff",
  "bike_diff",
  "wlk_diff",
  "wfh_diff",
  "othr_diff",
  key = "pred_diff",
  value = "value"
)

#Boxplot of predicted difference
ggplot(f6, aes(x = pred_diff, y = value)) + geom_boxplot()


## Mean Absolute Difference By Mode ###
mae_df = data.frame(
  mae_da = mean(abs(f5$da_diff)),
  mae_hov = mean(abs(f5$hov_diff)),
  mae_bus = mean(abs(f5$bus_diff)),
  mae_sub = mean(abs(f5$subway_diff)),
  mae_bike = mean(abs(f5$bike_diff)),
  mae_walk = mean(abs(f5$wlk_diff)),
  mae_wfh = mean(abs(f5$wfh_diff)),
  mae_othr = mean(abs(f5$othr_diff))
)

print(mae_df)

## RMSE by Mode ##
rmse_df = data.frame(
  rmse_da = sqrt(1 - cor(f5$da_p, f5$da_p1) ^ 2) * sd(f5$da_p),
  rmse_hov = sqrt(1 - cor(f5$hov_p, f5$hov_p1) ^ 2) *
    sd(f5$hov_p),
  rmse_bus = sqrt(1 - cor(f5$bus_p, f5$bus_p1) ^ 2) *
    sd(f5$bus_p),
  rmse_sub = sqrt(1 - cor(f5$subway_p, f5$subway_p1) ^
                    2) * sd(f5$subway_p),
  rmse_bike = sqrt(1 - cor(f5$bike_p, f5$bike_p1) ^ 2) *
    sd(f5$bike_p),
  rmse_walk = sqrt(1 - cor(f5$wlk_p, f5$wlk_p1) ^ 2) *
    sd(f5$wlk_p),
  rmse_wfh = sqrt(1 - cor(f5$wfh_p, f5$wfh_p1) ^ 2) *
    sd(f5$wfh_p),
  rmse_othr = sqrt(1 - cor(f5$othr_p, f5$othr_p1) ^ 2) *
    sd(f5$othr_p)
)

print(rmse_df)

## Percent Explained from Model Specification ##
R_sqrd = 100 * (1 - rmse_df)
colnames(R_sqrd) = c("R_DA",
                     "R_HOV",
                     "R_BUS",
                     "R_SUB",
                     "R_BIKE",
                     "R_WLK",
                     "R_WFH",
                     "R_OTHR")
print(R_sqrd)