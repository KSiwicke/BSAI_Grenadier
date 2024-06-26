library(dplyr)
library(DBI)
library(keyring)
library(ggplot2)

old_rpw <- read.csv("data/2020/rpw_2020.csv") |> 
  filter(!area == "GOA")

ggplot(old_rpw, aes(year, rpw)) +
  geom_point() + facet_wrap(~area) + theme_bw()

old_biom <- read.csv("data/2020/biomass_2020.csv")

ggplot(old_biom, aes(year, biomass)) +
  geom_point() + facet_wrap(~area) + theme_bw()


try <- old_biom |> filter(area == "EBS", year > 2009) |> 
  mutate(biomass = as.numeric(biomass))

summary(try)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username,
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username))

# Get longline survey RPWs
rpw <- dbGetQuery(channel_akfin,
                  "select    *
                from      afsc.lls_area_rpn_all_strata
                where     species_code = '21230' and
                          fmp_management_area = 'BSAI' and
                          country = 'United States'
                order by  year asc
                ") %>%
  rename_all(tolower)

cpue <- rpw %>%
  filter(year > 1991, year < YEAR + 1) %>%
  group_by(year, strata = council_management_area) %>%
  mutate(strata = ifelse(strata == 'Aleutians', 'AI',
                         ifelse(strata == 'Bering Sea', 'EBS', NA))) %>%
  summarize(cpue = sum(rpw, na.rm = TRUE),
            cv = sqrt(sum(rpw_var, na.rm = TRUE)) / cpue)

cpue_dat <- left_join(data.frame('year' = rep(unique(cpue$year), each = 2), 'strata' = rep(c('AI', 'EBS'), length(unique(cpue$year)))), cpue, by = c('year', 'strata'))

# Get bottom trawl survey biomass data
# Use the old data pull for comparison to the old model b/c GAP Products already removed 84/87 but old model included them
ai_biom <- dbGetQuery(channel_akfin, 
                   "select    *
                from      afsc.race_biomassstratumaigoa
                where     species_code = '21230' and 
                          survey = 'AI' and 
                          year > 1983
                order by  year asc
                ") %>% 
  rename_all(tolower) %>% 
  filter(year < (YEAR + 1))

ai_strata <- dbGetQuery(channel_akfin, 
                     "select    *
                from      afsc.race_goastrataaigoa
                where     survey = 'AI'
                ") %>% 
  rename_all(tolower)

ai_biom_old <- left_join(ai_biom, ai_strata, by = c("stratum"))  

# Using all data like previous model
biomass_5_old <- biom_old %>% 
  mutate(strata = ifelse(summary_depth == 992, "biomass_strata_1",
                         ifelse(summary_depth == 993, "biomass_strata_2",
                                ifelse(summary_depth == 994, "biomass_strata_3",
                                       ifelse(summary_depth == 995, "biomass_strata_4",
                                              ifelse(summary_depth == 996, "biomass_strata_5", NA)))))) %>% 
  filter(!is.na(strata)) %>%
  group_by(year, strata) %>% 
  summarize(n = n(), biomass = sum(stratum_biomass, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass)

biomass_dat_5_old <- left_join(data.frame('year' = rep(unique(biomass_5_old$year), each = 5), 
                                      'strata' = rep(unique(biomass_5_old$strata), length(unique(biomass_5_old$year)))), 
                           biomass_5_old, by = c('year', 'strata')) %>% 
  mutate(cv = ifelse(biomass == 0, 0.1, cv))

ebs = 98
ai = 52

biom <- dbGetQuery(channel_akfin, 
                   "select    *
                from      gap_products.akfin_biomass
                where     species_code = '21230' and 
                          survey_definition_id = 52 
                order by  year asc
                ") %>% 
  rename_all(tolower) %>% 
  filter(year < (YEAR + 1))

# # Using all data like previous model
biomass_5_new <- biom %>%
  mutate(strata = ifelse(area_id == 992, "biomass_strata_1",
                         ifelse(area_id == 993, "biomass_strata_2",
                                ifelse(area_id == 994, "biomass_strata_3",
                                       ifelse(area_id == 995, "biomass_strata_4",
                                              ifelse(area_id == 996, "biomass_strata_5", NA)))))) %>%
  filter(!is.na(strata)) %>%
  group_by(year, strata) %>%
  summarize(n = n(), biomass = sum(biomass_mt, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass)

biomass_dat_5_new <- left_join(data.frame('year' = rep(unique(biomass_5_new$year), each = 5), 
                                          'strata' = rep(unique(biomass_5_new$strata), length(unique(biomass_5_new$year)))), 
                               biomass_5_new, by = c('year', 'strata')) %>% 
  mutate(cv = ifelse(biomass == 0, 0.1, cv))

biomass_9 <- biom %>% 
  mutate(strata = ifelse(area_id %in% c(10:13, 110:112, 210, 310), 'WGOA (0-500 m)',
                         ifelse(area_id %in% c(20:35, 120:134, 220:232, 32, 320, 330), 'CGOA (0-500 m)',
                                ifelse(area_id %in% c(40:50, 140:151, 240:251, 340:351), 'EGOA (0-500 m)',
                                       ifelse(area_id == 410, 'WGOA (501-700 m)',
                                              ifelse(area_id == 510, 'WGOA (701-1000 m)',
                                                     ifelse(area_id %in% c(420, 430), 'CGOA (501-700 m)',
                                                            ifelse(area_id %in% c(520, 530), 'CGOA (701-1000 m)',
                                                                   ifelse(area_id %in% c(440, 450), 'EGOA (501-700 m)',
                                                                          ifelse(area_id %in% c(540, 550), 'EGOA (701-1000 m)', NA)))))))))) %>% 
  filter(!is.na(strata)) %>% 
  group_by(year, strata) %>% 
  summarize(n = n(), biomass = sum(biomass_mt, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

biomass_dat_9 <- left_join(data.frame('year' = rep(unique(biomass_9$year), each = 9), 'strata' = rep(unique(biomass_9$strata), length(unique(biomass_9$year)))), biomass_9, by = c('year', 'strata')) %>% 
  mutate(cv = ifelse (cv == 0, 0.1, cv))

model_yrs <- 1984:YEAR

# This is the data that is brought into rema
model_dat <- list('biomass_dat_5_old' = biomass_dat_5_old,
                  'biomass_dat_5_new' = biomass_dat_5_new,
                  'biomass_dat_9' = biomass_dat_9,
                  'cpue_dat' = cpue_dat, 
                  'model_yrs' = model_yrs)
