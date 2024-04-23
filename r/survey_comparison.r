library(dplyr)
library(lubridate)
library(DBI)
library(keyring)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username,
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username))

# Looking at catch in BTS across region by depth...how well is each survey sampling shortraker???
catch <- dbGetQuery(channel_akfin, 
                    "select    *
                from      afsc.race_catchaigoa
                where     species_code = '21230' and 
                          region = 'GOA' 

                ") %>% 
  rename_all(tolower)

haul <- dbGetQuery(channel_akfin, 
                   "select    *
                from      afsc.race_haulaigoa
                where     region = 'GOA' 
                ") %>% 
  rename_all(tolower) %>% 
  filter(abundance_haul == "Y", year(start_time) > 1989, year(start_time) < YEAR + 1, haul_type == 3) %>% 
  mutate(strata = ifelse(stratum %in% c(10:13, 110:112, 210, 310, 410, 510), 'WGOA',
                         ifelse(stratum %in% c(20:35, 120:134, 220:232, 32, 320, 330, 420, 430, 520, 530), 'CGOA',
                                ifelse(stratum %in% c(40:50, 140:151, 240:251, 340:351, 440, 450, 540, 550), 'EGOA', NA))))

cat_dat <- left_join(haul, catch, by = c("hauljoin")) %>% 
  mutate(number_fish = ifelse(is.na(number_fish), 0, number_fish),
    strata = ifelse(stratum %in% c(10:13, 110:112, 210, 310, 410, 510), 'WGOA',
                         ifelse(stratum %in% c(20:35, 120:134, 220:232, 32, 320, 330, 420, 430, 520, 530), 'CGOA',
                                ifelse(stratum %in% c(40:50, 140:151, 240:251, 340:351, 440, 450, 540, 550), 'EGOA', NA))))

ggplot(cat_dat) + 
  geom_density(aes(bottom_depth), fill = "grey60", alpha = 0.8) + 
  geom_density(aes(bottom_depth, weight = number_fish), fill = "red", alpha = 0.8) + 
  facet_grid(year(start_time)~factor(strata, levels=c('WGOA', 'CGOA', 'EGOA'))) +
  theme_bw() +
  ylab("Density") +
  xlab("Depth (m)")

# All years combined
ggplot(haul) + 
  geom_density(aes(bottom_depth), fill = "grey60", alpha = 0.8) + 
  geom_density(data = cat_dat, aes(bottom_depth, weight = number_fish), fill = "red", alpha = 0.8) + 
  facet_grid(~factor(strata, levels=c('WGOA', 'CGOA', 'EGOA'))) +
  theme_bw() +
  ylab("Density") +
  xlab("Depth (m)") +
  scale_x_continuous(expand=c(0,0), limits = c(0, 1200), breaks=seq(0, 1200, 250))

ggsave(file = paste0("results/", YEAR, "/BTS_depth_effort.png"), height = 2.5, width = 6, dpi=600)

# Looking at catch in LLS across region by depth 
ll_dep <- dbGetQuery(channel_akfin, 
                     "select    *
                from      afsc.lls_depth_summary_view
                where     exploitable = 1 and 
                          year > 1991 and
                          country = 'United States'
                order by  year asc
                ") %>% 
  rename_all(tolower) %>% 
  filter(year < YEAR + 1, !council_sablefish_management_area == "Aleutians", !council_sablefish_management_area == "Bering Sea") %>% 
  mutate(region = ifelse(council_sablefish_management_area == "Central Gulf of Alaska", "CGOA",
                         ifelse(council_sablefish_management_area == "Western Gulf of Alaska", "WGOA",
                                "EGOA")))

ll_catch <- dbGetQuery(channel_akfin, 
                       "select    *
                from      afsc.lls_catch_summary_view
                where     species_code = '21230' and 
                          year > 1991 and
                          year < 2022 and
                          exploitable = 1 and 
                          country = 'United States'
                order by  year asc
                ") %>% 
  rename_all(tolower) %>% 
  filter(!council_sablefish_management_area == "Aleutians", !council_sablefish_management_area == "Bering Sea") %>% 
  select("cruise_number", "station_number", "hachi", "catch_freq")

ll_dat <- left_join(ll_dep, ll_catch, by = c("cruise_number", "station_number", "hachi")) %>%
  filter(rpw_flag == 1, ineffective < 6) %>% 
  mutate(catch_freq = ifelse(is.na(catch_freq), 0, catch_freq))
  
ggplot(ll_dat) + 
  geom_density(aes(intrpdep), fill = "grey60", alpha = 0.8) + 
  geom_density(aes(intrpdep, weight = catch_freq), fill = "red", alpha = 0.8) + 
  facet_grid(year~factor(region, levels=c('WGOA', 'CGOA', 'EGOA'))) +
  theme_bw() +
  ylab("Density") +
  xlab("Depth (m)")

# All years combined
ggplot(ll_dat) + 
  geom_density(aes(intrpdep), fill = "grey60", alpha = 0.8) + 
  geom_density(aes(intrpdep, weight = catch_freq), fill = "red", alpha = 0.8) + 
  facet_grid(~factor(region, levels=c('WGOA', 'CGOA', 'EGOA'))) +
  theme_bw() +
  ylab("Density") +
  xlab("Depth (m)") +
  scale_x_continuous(expand=c(0,0), limits = c(0, 1200), breaks=seq(0, 1200, 250))

ggsave(file = paste0("results/", YEAR, "/LLS_depth_effort.png"), height = 2.5, width = 6, dpi=600)

cowplot::plot_grid(ggplot(haul) + 
                     geom_density(aes(bottom_depth), fill = "grey60", alpha = 0.8) + 
                     geom_density(data = cat_dat, aes(bottom_depth, weight = number_fish), fill = "red", alpha = 0.8) + 
                     facet_grid(~factor(strata, levels=c('WGOA', 'CGOA', 'EGOA'))) +
                     theme_bw() +
                     labs(y = "Density (BTS)", x = "Depth (m)") +
                     scale_x_continuous(expand=c(0,0), limits = c(0, 1200), breaks=seq(0, 1200, 250)),
                   ggplot(ll_dat) + 
                     geom_density(aes(intrpdep), fill = "grey60", alpha = 0.8) + 
                     geom_density(aes(intrpdep, weight = catch_freq), fill = "blue", alpha = 0.8) + 
                     facet_grid(~factor(region, levels=c('WGOA', 'CGOA', 'EGOA'))) +
                     theme_bw() +
                     labs(y = "Density (LLS)", x = "Depth (m)", fill = "ddd") +
                     scale_x_continuous(expand=c(0,0), limits = c(0, 1200), breaks=seq(0, 1200, 250)),
                   ncol = 1)

ggsave(file = paste0("results/", YEAR, "/combo_depth_effort.png"), height = 5, width = 6, dpi=600)
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
