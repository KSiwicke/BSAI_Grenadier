library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DBI)
library(keyring)
library(cowplot)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username[1],
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username[1]))

# Looking at catch in BTS across region by depth...how well is each survey sampling shortraker???
catch <- dbGetQuery(channel_akfin, 
                    "select    *
                    from      gap_products.akfin_catch
                    where     species_code = '21230'
                ") |> 
  rename_all(tolower)

haul <- dbGetQuery(channel_akfin, 
                   "select  CRUISEJOIN, DEPTH_GEAR_M, HAULJOIN, LATITUDE_DD_START, PERFORMANCE,
                            LONGITUDE_DD_END, DISTANCE_FISHED_KM, NET_HEIGHT_M, NET_WIDTH_M
                    from    gap_products.akfin_haul
                    where performance = 0
                ") |>  
  rename_all(tolower) 

cruise <- dbGetQuery(channel_akfin, 
                     "select    CRUISEJOIN, YEAR, SURVEY_DEFINITION_ID
                      from      gap_products.akfin_cruise
                      where     survey_definition_id = 52
                      ") |>  
  rename_all(tolower) 

haul_dat <- left_join(haul, cruise) |> 
  mutate(region = ifelse(survey_definition_id == 52, "AI", NA)) |>
  filter(!is.na(region))

# Function to scale cpue from 0 to 1 to compare across surveys
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

bt_dat <- left_join(haul_dat, catch, by = c("hauljoin")) %>% 
  mutate(catch_freq = ifelse(is.na(count), 0, count),
         effort = distance_fished_km) |> 
  filter(!is.na(depth_gear_m), depth_gear_m > 0) |> 
  mutate(dep = ifelse(depth_gear_m > 1000, 1001, depth_gear_m)) |> 
  group_by(depth = cut(dep, breaks = seq(0, 1050, 50), labels = c(seq(50, 1000, 50), ">1000"))) |> 
  summarize(N_hauls = n(), eff = sum(effort), freq = sum(catch_freq)) |> 
  filter(N_hauls > 10)  |> 
  mutate(survey = "BTS", cpue = range01(freq / eff))
         
# Looking at catch in LLS across region by depth 
ll_dep <- dbGetQuery(channel_akfin, 
                     "select    *
                from      afsc.lls_depth_summary_view
                where     exploitable = 1 and 
                          year > 1991 and
                          council_sablefish_management_area = 'Aleutians' and 
                          country = 'United States'
                order by  year asc
                ") |> 
  rename_all(tolower) |> 
  mutate(region = ifelse(council_sablefish_management_area == "Aleutians", "AI",
                         ifelse(council_sablefish_management_area == "Bering Sea", "EBS",
                                ifelse(council_sablefish_management_area %in% c("Central Gulf of Alaska", "Western Gulf of Alaska", "Eastern Gulf of Alaska"),
                                       "GOA", NA))))
                                                                                
ll_catch <- dbGetQuery(channel_akfin, 
                       "select    *
                from      afsc.lls_catch_summary_view
                where     species_code = '21230' and 
                          year > 1991 and
                          exploitable = 1 and
                          council_sablefish_management_area = 'Aleutians' and 
                          country = 'United States'
                order by  year asc
                ") |> 
  rename_all(tolower) |> 
  select("cruise_number", "station_number", "hachi", "catch_freq")

DBI::dbDisconnect(channel_akfin)

ll_dat <- left_join(ll_dep, ll_catch, by = c("cruise_number", "station_number", "hachi")) %>%
  filter(rpw_flag == 1, ineffective < 6) %>% 
  mutate(catch_freq = ifelse(is.na(catch_freq), 0, catch_freq)) |> 
  filter(intrpdep > 0) |> 
  mutate(dep = ifelse(intrpdep > 1000, 1001, intrpdep)) |> 
  group_by(depth = cut(dep, breaks = seq(0, 1050, 50), labels = c(seq(50, 1000, 50), ">1000"))) |> 
  summarize(eff = n(), freq = sum(catch_freq)) |> 
  mutate(survey = "LLS", cpue = range01(freq / eff))

cpue_dat <- bind_rows(bt_dat, ll_dat)
  
ggplot(cpue_dat, aes(depth, cpue, col = survey)) + 
  geom_point(position = position_dodge(0.6)) + 
  theme_bw() +
  xlab("Depth bin") +
  ylab("Scaled No. per unit effort") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(file = paste0("results/ai_", YEAR, "/surveys_by_depth_bin.png"), height = 5, width = 6, dpi=600)

# Now look at the lengths by survey...
lls.gren.len2 <- lls.gren.len %>% 
  filter(!length == 999) %>% 
  mutate(region = ifelse(council_sablefish_management_area == "Central Gulf of Alaska", "CGOA",
                         ifelse(council_sablefish_management_area == "Western Gulf of Alaska", "WGOA", "EGOA")))

# Group the LL numbers by year/region/length
ll.len.agg <- lls.gren.len2 %>% 
  group_by(year, length, region) %>% 
  summarize(freq = sum(rpn, na.rm = T)) 

ll.len.disagg <- ll.len.agg %>% 
  slice(rep(1:n(), freq)) %>% 
  select(-freq)

ll.means <- ll.len.disagg %>% group_by(year, region) %>% 
  summarize(mean = mean(length), sd = sd(length)) 

ll.means2 <- ll.len.disagg %>% group_by(region) %>% 
  summarize(mean = mean(length), sd = sd(length)) 

# ll.len = merge(ll.len.disagg, ll.means, by=c("year", "region"))

# Plot all regions 
ggplot() +
  geom_histogram(data=ll.len.disagg, aes(x=length, y=after_stat(density), fill = region),
                 position = 'identity', alpha=0.5, binwidth=1, col="black") +
  theme(legend.position = "top") +
  # scale_fill_discrete(type = c('red', 'blue', 'orange')) +
  facet_wrap(~factor(region, levels=c('WGOA', 'CGOA', 'EGOA')), ncol = 1) +
  xlab("Length (cm)") +
  ylab("Length composition by region") +
  geom_text(data=ll.means2, aes(x=c(51),  y=c(0.043), label=paste0(region, " mean length: ", format(round(mean, digits=1), nsmall = 1) , " cm"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.12)) +
  scale_x_continuous(expand=c(0,5), breaks=seq(15,65,10), limits=c(15,70)) +
  # facet_wrap(~survey, ncol=1) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

ggsave(file = paste0("results/", YEAR, "/LLS_reg_len.png"), height = 8, width = 6, dpi=600)

# BTS
bts.gren.len2 <- bts.gren.len %>% 
  mutate(region = ifelse(stratum %in% c(10:13, 110:112, 210, 310, 410, 510), 'WGOA',
                         ifelse(stratum %in% c(20:35, 120:134, 220:232, 32, 320, 330, 420, 430, 520, 530), 'CGOA',
                                ifelse(stratum %in% c(40:50, 140:151, 240:251, 340:351, 440, 450, 540, 550), 'EGOA', NA))))

#####################
# Group the BTS numbers by year/length
len.agg <- bts.gren.len2 %>% 
  mutate(length = length / 10) %>% 
  group_by(year, length, region) %>% 
  summarize(freq = sum(total))

len.disagg <- len.agg %>% 
  slice(rep(1:n(), freq)) %>% 
  select(-freq)

means <- len.disagg %>% group_by(year, region) %>% 
  summarize(mean = mean(length), sd = sd(length))

means2 <- len.disagg %>% group_by(region) %>% 
  summarize(mean = mean(length), sd = sd(length))

# len = merge(len.disagg, means, by=c("year", "region"))

# Plot all regions 
ggplot() +
  geom_histogram(data=len.disagg, aes(x=length, y=after_stat(density), fill = region),
                 position = 'identity', alpha=0.5, binwidth=1, col="black") +
  theme(legend.position = "top") +
  facet_wrap(~factor(region, levels=c('WGOA', 'CGOA', 'EGOA')), ncol = 1) +
  xlab("Length (cm)") +
  ylab("Length composition by region") +
  geom_text(data=means2, aes(x=c(82),  y=c(0.055), label=paste0(region, " mean length: ", format(round(mean, digits=1), nsmall = 1) , " cm"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.07)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

ggsave(file = paste0("results/", YEAR, "/BTS_reg_len.png"), height = 8, width = 6, dpi=600)

# Combine regional lengths for BTS and LLS...
len.disagg$survey = "BTS"
ll.len.disagg$survey = "LLS"

all = rbind(len.disagg, ll.len.disagg)

ll.means2$survey = "LLS"
means2$survey = "BTS"
all.mean = rbind(ll.means2, means2)

ggplot() +
  geom_histogram(data=all, aes(x=length, y=after_stat(density), fill = survey),
                 position = 'identity', alpha=0.5, binwidth=1, col="black") +
  theme(legend.position = "top") +
  scale_fill_discrete(type = c('red', 'blue')) +
  facet_wrap(~factor(region, levels=c('WGOA', 'CGOA', 'EGOA')), ncol = 1) +
  xlab("Length (cm)") +
  ylab("Length composition by survey") +
  labs(fill = "Survey") +
  # geom_text(data=all.mean, aes(x=c(23,23),  y=c(0.045, .04), label=paste0(survey, " mean length: ", format(round(mean, digits=1), nsmall = 1) , " cm"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.067)) +
  scale_x_continuous(expand=c(0,5), breaks=seq(5,115,10)) +
  # facet_wrap(~survey, ncol=1) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), panel.grid.minor = element_blank())

ggsave(file = paste0("results/", YEAR, "/all_len_by_reg.png"), height = 8, width = 6, dpi=600)

# Time series with the annual mean length by survey
ll.means$survey = "LLS"
means$survey = "BTS"
ll.means$year = ll.means$year + 0.25
means$year = means$year - 0.25
all.mean = rbind(ll.means, means)

ggplot(all.mean, aes(year, mean, col = survey)) +
  geom_point(size = 2) +
  scale_color_discrete(type = list(c("red", "blue"))) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
  facet_grid(~factor(region, levels=c('WGOA', 'CGOA', 'EGOA'))) +
  labs(y = "Mean length (cm)", x = "Year", col = "Survey") +
  scale_x_continuous(breaks=seq(1990,2020,10)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), panel.grid.minor = element_blank())

ggsave(file = paste0("results/", YEAR, "/MeanLengths_TS.png"), height = 5, width = 10, dpi=600)
