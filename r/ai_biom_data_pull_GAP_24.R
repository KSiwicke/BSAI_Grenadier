library(dplyr)
library(DBI)
library(keyring)
library(ggplot2)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                dsn = db,
                uid = keyring::key_list(db)$username,
                pwd =  keyring::key_get(db, keyring::key_list(db)$username))

# areas <- dbGetQuery(channel_akfin,
#                     "select    *
#                 from      gap_products.akfin_area
#                 where     survey_definition_id = 52 and
#                           area_type = 'STRATUM' 
#                 ") %>% 
#   rename_all(tolower)
# 
# hauls <- dbGetQuery(channel_akfin,
#                     "select    *
#                 from      gap_products.akfin_haul
#                 where     stratum > 210 and stratum < 795
#                 ") %>% 
#   rename_all(tolower) |> 
#   filter(longitude_dd_start < -170.001 | longitude_dd_start > 0) |> 
#   mutate(longitude_dd_start = ifelse(longitude_dd_start < 0, longitude_dd_start + 360, longitude_dd_start))
# 
# ggplot(hauls, aes(longitude_dd_start, latitude_dd_start, col = factor(stratum))) + geom_point() +
#   geom_point(data = hauls |> filter(stratum == 594), aes(longitude_dd_start, latitude_dd_start), col = "red", size = 3)

# Get longline survey RPWs

# First all of AI which extrapolates out west
cpue <- dbGetQuery(channel_akfin,
                   "select    *
                from      afsc.lls_area_rpn_all_strata
                where     species_code = '21230' and
                          fmp_management_area = 'BSAI' and
                          council_sablefish_management_area = 'Aleutians' and
                          exploitable = 1 and
                          country = 'United States'
                order by  year asc
                                ") %>%
  rename_all(tolower)

ggplot(cpue |> filter(geographic_area_name %in% c("NE Aleutians slope", "SE Aleutians slope")), aes(year, rpn)) +
  geom_point() + 
  facet_wrap(~geographic_area_name, nrow = 2) + 
  geom_linerange(aes(ymin = rpn - sqrt(rpn_var), ymax = rpn + sqrt(rpn_var))) +
  labs(x = "Year", y = "Relative Population Weight") +
  theme_bw()

ggplot(cpue |> filter(geographic_area_name %in% c("NE Aleutians slope", "SE Aleutians slope")), aes(year, rpw/100000)) +
  geom_point() + 
  facet_wrap(~geographic_area_name, nrow = 2) + 
  geom_linerange(aes(ymin = rpw/100000 - sqrt(rpw_var)/100000, ymax = rpw/100000 + sqrt(rpw_var)/100000)) +
  labs(x = "Year", y = "Relative Population Weight") +
  theme_bw()

ggsave(file = paste0(out_path, "_LLS_AI_index.png"), dpi = 300, units = "in", height = 6, width = 4)

cpue_dat <- cpue |>
  group_by(year) |>
  summarize(cpue = sum(rpw, na.rm = TRUE),
            cv = sqrt(sum(rpw_var, na.rm = TRUE)) / cpue) |>
  mutate(strata = "AI")

# Then version that just has data
# cpue <- dbGetQuery(channel_akfin,
#                 "select    *
#                 from      afsc.lls_area_stratum_rpn
#                 where     species_code = '21230' and
#                           council_sablefish_management_area = 'Aleutians' and
#                           exploitable = 1 and
#                           country = 'United States'
#                 order by  year asc
#                 ") %>%
#   rename_all(tolower) |> 
#   filter(!stratum %in% c("1", "2a", "8")) |> 
#   mutate(group = ifelse(stratum %in% c("2b", "3", "4"), "shallow",
#                          ifelse(stratum %in% c("6", "7"), "deep", "split")))
# 
# 
# # DBI::dbDisconnect(channel_akfin)
# 
# rpw_split_shallow <- cpue |> filter(group == "split") |> 
#   mutate(rpw = rpw * 0.5, group = "shallow")
# 
# rpw_split_deep <- cpue |> filter(group == "split") |> 
#   mutate(rpw = rpw * 0.5, group = "deep")
# 
# rpw <- cpue |> filter(!group == "split") |> 
#   bind_rows(rpw_split_shallow) |> 
#   bind_rows(rpw_split_deep) |> 
#   group_by(year, area_code, group) |> 
#   summarize(cpue = sum(rpw, na.rm = TRUE))
# 
# ggplot(rpw, aes(year, cpue)) + 
#   geom_line() +
#   facet_wrap(area_code~group)

# cpue <- rpw %>%
#   filter(year > 1991, year < YEAR + 1) %>%
#   group_by(year, strata = council_management_area) %>%
#   mutate(strata = ifelse(strata == 'Aleutians', 'AI',
#                          ifelse(strata == 'Bering Sea', 'EBS', NA))) %>%
#   summarize(cpue = sum(rpw, na.rm = TRUE),
#             cv = sqrt(sum(rpw_var, na.rm = TRUE)) / cpue)
# 
# cpue_dat <- left_join(data.frame('year' = rep(unique(cpue$year), each = 2), 'strata' = rep(c('WGOA', 'CGOA', 'EGOA'), length(unique(cpue$year)))), cpue, by = c('year', 'strata'))
# 
# ggplot(cpue, aes(year, cpue)) +
#   geom_line() +
#   facet_wrap(~strata)

# ggplot(cpue_dat, aes(year, cpue)) +
#    geom_line() +
#    facet_wrap(~strata)

# Get bottom trawl survey biomass data
biom <- dbGetQuery(channel_akfin, 
                   "select    *
                from      gap_products.akfin_biomass
                where     species_code = '21230' and 
                          survey_definition_id = 52 
                order by  year asc
                ") %>% 
  rename_all(tolower)

DBI::dbDisconnect(channel_akfin)

# Using all data like previous model
ai_biom <- biom %>% 
  filter(area_id == 99904) %>% 
  group_by(year) %>% 
  summarize(n = sum(n_haul), biomass = sum(biomass_mt, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

ggplot(ai_biom, aes(year, biomass / 10000)) + 
  geom_point() + 
  geom_linerange(aes(ymin = biomass /10000 - biomass/10000*cv, ymax = biomass/10000 + biomass/10000*cv)) +
  labs(x = "Year", y = "Biomass (Ten thousand t)") +
  theme_bw()

biomass <- biom %>% 
  mutate(strata = ifelse(area_id == 299, 'WAI (1-500 m)',
                         ifelse(area_id == 3499, 'CAI (1-500 m)',
                                ifelse(area_id == 5699, 'EAI (1-500 m)',
                                       ifelse(area_id == 799, 'SBS (1-500 m)', NA))))) %>% 
  filter(!is.na(strata)) %>% 
  group_by(year, strata) %>% 
  summarize(n = sum(n_haul), biomass = sum(biomass_mt, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

ggplot(biomass, aes(year, biomass / 10000)) + 
  geom_point() + 
  facet_wrap(~factor(strata, levels = c("WAI (1-500 m)", "CAI (1-500 m)", "EAI (1-500 m)", "SBS (1-500 m)")), nrow = 1) + 
  geom_linerange(aes(ymin = biomass /10000 - biomass/10000*cv, ymax = biomass/10000 + biomass/10000*cv)) +
  labs(x = "Year", y = "Biomass (Ten thousand t)") +
  theme_bw()

ggsave(file = paste0(out_path, "_BTS_AI_index.png"), dpi = 300, units = "in", height = 3, width = 10)

biomass <- biomass |> filter(!strata == 'SBS (1-500 m)')
biomass_dat <- left_join(data.frame('year' = rep(unique(biomass$year), each = 3), 'strata' = rep(unique(biomass$strata), length(unique(biomass$year)))), biomass, by = c('year', 'strata')) %>% 
  mutate(cv = ifelse (cv == 0, 0.1, cv))

         # biomass = ifelse(biomass == 0, 0.0001, biomass)) 

model_yrs <- 1990:YEAR

# This is the data that is brought into rema
model_dat <- list('biomass_dat' = biomass_dat, 'cpue_dat' = cpue_dat, 
                  'model_yrs' = model_yrs)
