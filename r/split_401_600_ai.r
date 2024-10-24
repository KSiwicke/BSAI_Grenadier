library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DBI)
library(keyring)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username[1],
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username[1]))

# Looking at catch in LLS across Aleutians in the 401-600 m strata (#5)
ll_dep <- dbGetQuery(channel_akfin, 
                     "select    *
                from      afsc.lls_depth_summary_view
                where     exploitable = 1 and 
                          year > 1991 and
                          council_sablefish_management_area = 'Aleutians' and
                          stratum = 5 and
                          country = 'United States'
                order by  year asc
                ") |> 
  rename_all(tolower) 

ll_catch <- dbGetQuery(channel_akfin, 
                       "select    *
                from      afsc.lls_catch_summary_view
                where     species_code = '21230' and 
                          year > 1991 and
                          stratum = 5 and
                          council_sablefish_management_area = 'Aleutians' and
                          exploitable = 1 and 
                          country = 'United States'
                order by  year asc
                ") |> 
  rename_all(tolower) |> 
  select("cruise_number", "station_number", "hachi", "catch_freq")

DBI::dbDisconnect(channel_akfin)

strat_500 <- left_join(ll_dep, ll_catch, by = c("cruise_number", "station_number", "hachi")) %>%
  filter(rpw_flag == 1, ineffective < 6) %>% 
  mutate(catch_freq = ifelse(is.na(catch_freq), 0, catch_freq),
         split = ifelse(intrpdep > 500, 'deep', 'shallow')) |> 
  group_by(cruise_number, split) |> 
  summarize(split_skate = n(), split_freq = sum(catch_freq)) |> 
  pivot_wider(id_cols = c(cruise_number), names_from = split, values_from = c(split_freq, split_skate)) |> 
  mutate(deep_cpue = split_freq_deep / split_skate_deep,
         shallow_cpue = split_freq_shallow / split_skate_shallow,
         dp_2_sh_r = deep_cpue / shallow_cpue,
         per_shallow = shallow_cpue / (shallow_cpue + deep_cpue))
  
ggplot(strat_500, aes(shallow_cpue, deep_cpue)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  scale_x_continuous(limits = c(0, 21), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 21), expand = c(0, 0)) +
  theme_bw()

ggsave(file = paste0("results/ai_", YEAR, "/split_401_600.png"), height = 6, width = 6, dpi=600)

density_500 <- left_join(ll_dep, ll_catch, by = c("cruise_number", "station_number", "hachi")) %>%
  filter(rpw_flag == 1, ineffective < 6) %>% 
  mutate(catch_freq = ifelse(is.na(catch_freq), 0, catch_freq)) |> 
  group_by(intrpdep) |> 
  summarize(skates = n(), freq = sum(catch_freq))
  

# |> 
#   group_by(cruise_number, split) |> 
#   summarize(split_skate = n(), split_freq = sum(catch_freq)) |> 
#   pivot_wider(id_cols = c(cruise_number), names_from = split, values_from = c(split_freq, split_skate)) |> 
#   mutate(deep_cpue = split_freq_deep / split_skate_deep,
#          shallow_cpue = split_freq_shallow / split_skate_shallow,
#          dp_2_sh_r = deep_cpue / shallow_cpue)
# 
# ggplot(density_500) + 
#   # geom_density(aes(freq / skates), fill = "grey60", alpha = 0.8) + 
#   geom_density(aes(intrpdep, weight = (freq / skates)), fill = "red", alpha = 0.8) +
#   theme_bw() +
#   ylab("Density") +
#   xlab("Depth (m)")
