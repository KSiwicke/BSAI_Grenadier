library(dplyr)
library(DBI)
library(keyring)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username,
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username))

lls.gren.len <- dbGetQuery(channel_akfin, 
                          "select    *
                from      afsc.lls_length_rpn_by_area_all_strata
                where     species_code = '21230' 
                order by  year asc") %>% 
  rename_all(tolower) %>% 
  filter(year > 1991, !council_sablefish_management_area == "Aleutians",
         !council_sablefish_management_area == "Bering Sea") %>% 
  write_csv(paste0(dat_path, "/goa_gren_lls_lengths", YEAR, ".csv"))

bts.gren.len <- dbGetQuery(channel_akfin, 
                          "select    *
                from      afsc.race_sizestratumaigoa 
                where     species_code = '21230'") %>% 
  rename_all(tolower) %>% 
  filter(year > 1989, survey == "GOA") %>% 
  write_csv(paste0(dat_path, "/goa_gren_bts_lengths", YEAR, ".csv"))

# get age data to population
# bts.gren.age <- dbGetQuery(channel_akfin, 
#                           "select    *
#                 from      afsc.race_agecomptotalaigoa 
#                 where     species_code = '21230'") %>% 
#   rename_all(tolower) %>% 
#   filter(survey == "GOA") %>% 
#   write_csv(paste0(dat_path, "/goa_gren_bts_ages", YEAR, ".csv"))

# Age sample sizes
# bts.gren.spec <- dbGetQuery(channel_akfin, 
#                          "select    *
#                 from      afsc.race_specimenaigoa 
#                 where     species_code = '21230'") %>% 
#   rename_all(tolower) %>% 
#   filter(region == "GOA", age > 0) %>% 
#   mutate(survey_year = (cruise - 1) / 100) %>% 
#   group_by(survey_year) %>% 
#   summarize(Num = n())
  
#Fishery Lengths
fsh.gren.len <- dbGetQuery(channel_akfin,
                          "select    *
                from      norpac.debriefed_length
                where     species = 82 and 
                          nmfs_area > 609 and
                          nmfs_area < 651 and
                          year >= 1989") %>% 
  rename_all(tolower) %>% 
  mutate(region = ifelse(nmfs_area == 610, 'WGOA', 
                         ifelse(nmfs_area == 620 | nmfs_area == 630, 'CGOA',
                                ifelse(nmfs_area == 640 | nmfs_area == 650, 'EGOA', NA)))) %>% 
  write_csv(paste0(dat_path, "/goa_sr_fishery_lengths", YEAR, ".csv"))

