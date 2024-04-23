# =========================================================================================
#
# Shortraker assessment Length figures
#
# =========================================================================================
# Pull length data
source("code/other_data_pull.r")

####################
# Group the LL numbers by year/length
ll.len = lls.gren.len %>% 
  filter(!length == 999) %>% 
  group_by(year, length) %>% 
  summarize(freq = sum(rpn, na.rm = T))

ll.len$calc = ll.len$freq*ll.len$length

ll.means = ll.len %>% group_by(year) %>% 
  summarize(tot = sum(freq), l_calc = sum(calc))

ll.means$mean = ll.means$l_calc/ll.means$tot
ll.len = merge(ll.len, ll.means, by=c("year"))

ll.len %>%  
  ggplot(aes(x=length, y=after_stat(density), weighted.mean=freq)) +
  geom_histogram(alpha=0.25, binwidth=1, col="black") +
  facet_wrap(~year, ncol=3) +
  theme(legend.position = "top") +
  xlab("Length (cm)") +
  ylab("Proportion of LL survey RPNs") +
  geom_text(aes(x=45, y=0.14, label=year)) +
  geom_text(aes(x=45, y=0.09, label=paste0("(", format(round(mean, digits=1), nsmall = 1) , " cm)"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.2)) +
  scale_x_continuous(breaks=seq(15,55,10), limits=c(12,57)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

ggsave(file=paste0("results/", YEAR, "/Gren_LLS_Lengths.png"), height = 15, width = 12, dpi=600)

ggplot(ll.len, aes(year, mean)) + geom_point() + geom_line()

ll.len$survey = "LLS"

#####################
# Group the BTS numbers by year/length
len = bts.gren.len %>% 
  mutate(length = length / 10) %>% 
  group_by(year, length) %>% 
  summarize(freq = sum(total)) 
  
len$calc = len$freq*len$length

means = len %>% group_by(year) %>% 
  summarize(tot = sum(freq), l_calc = sum(calc))
  
means$mean = means$l_calc/means$tot
len = merge(len, means, by=c("year"))

len %>%  
        ggplot(aes(x=length, y=after_stat(density), weighted.mean=freq)) +
        # theme_linedraw() +
        geom_histogram(alpha=0.25, binwidth=1, col="black") +
        facet_wrap(~year, ncol=2) +
        theme(legend.position = "top") +
        xlab("Length (cm)") +
        ylab("Proportion of trawl survey population") +
        geom_text(aes(x=45, y=0.3, label=year)) +
        geom_text(aes(x=45, y=0.2, label=paste0("(", format(round(mean, digits=1), nsmall = 1) , " cm)"))) +
        scale_y_continuous(expand=c(0,0), limits=c(0,0.42)) +
        scale_x_continuous(breaks=seq(15,55,10), limits=c(12,57)) +
        theme_bw() +
        theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
                panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

ggsave(file = paste0("results/", YEAR, "/Gren_BTS_Lengths.png"), height = 10, width = 7, dpi=600)

len$survey = "BTS"

# Look at a time series of the mean length by surveys
ggplot(ll.means, aes(year, mean))  + 
  geom_point(size=4) +
  geom_point(data=means, aes(x=year, y=mean), size=4, pch=21) +
  ylab("Mean length (cm)") +
  scale_x_continuous(breaks=seq(1990,2020,5)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

ggsave(file = paste0("results/", YEAR, "/Gren_Time_Series_length_Comp.png"), height = 3.5, width = 6, dpi=600)

# IS there a relationship between mean of LL lengths and mean of BTS lengthS?
comb = merge(ll.means, means, by="year")

ggplot(comb, aes(mean.x, mean.y)) + 
  geom_point()

# YES relationship present

# Summary of entire survey datasets
all.bts = bts.gren.len %>% 
  mutate(length = length / 10) %>% 
  group_by(length) %>% 
  summarize(freq = sum(total))

all.bts$calc = as.numeric(all.bts$freq)*as.numeric(all.bts$length)
all.bts$survey = "BTS"

# all.bts = all.bts[ , c(3,2,4,5)]

bts.mean = all.bts %>% summarize(tot = sum(freq), l_calc = sum(calc))

bts_mean = data.frame("Mean" = bts.mean$l_calc/bts.mean$tot)

all.bts %>%  
  ggplot(aes(x=length, y=after_stat(density), weighted.mean=freq)) +
  geom_histogram(alpha=0.25, binwidth=1, col="black") +
  theme(legend.position = "top") +
  xlab("Length (cm)") +
  ylab("Proportion of trawl survey population") +
  geom_text(aes(x=10, y=0.05, label=paste0("(", format(round(bts_mean, digits=1), nsmall = 1) , " cm)"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.112)) +
  scale_x_continuous(breaks=seq(10,110,10), limits = c(5, 55)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

all.ll = lls.gren.len %>% 
  filter(!length == 999) %>% 
  group_by(length) %>% 
  summarize(freq = sum(rpn, na.rm = TRUE))

all.ll$calc = as.numeric(all.ll$freq)*as.numeric(all.ll$length)
all.ll$survey = "LLS"

ll.mean = all.ll %>% 
  summarize(tot = sum(freq), l_calc = sum(calc))

ll_mean = data.frame("Mean" = ll.mean$l_calc/ll.mean$tot)

all.ll %>%  
  ggplot(aes(x=length, y=after_stat(density), weighted.mean=freq)) +
  geom_histogram(alpha=0.25, binwidth=1, col="black") +
  theme(legend.position = "top") +
  xlab("Length (cm)") +
  ylab("Proportion of LL survey RPNs") +
  geom_text(aes(x=10, y=0.06, label=paste0("(", format(round(ll_mean, digits=1), nsmall = 1) , " cm)"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.114)) +
  scale_x_continuous(breaks=seq(10,110,10), limits = c(5, 55)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

# Combine for plot
all = rbind(all.bts, all.ll)

ll_mean$survey = "LLS"
bts_mean$survey = "BTS"
all.mean = rbind(ll_mean, bts_mean)

ggplot() +
  geom_histogram(data=all, aes(x=length, y=after_stat(density), weighted.mean=freq, fill = survey),
                 position = 'identity', alpha=0.5, binwidth=1, col="black") +
  theme(legend.position = "top") +
  scale_fill_discrete(type = c('red', 'blue')) +
  xlab("Length (cm)") +
  ylab("Length composition by survey") +
  geom_text(data=all.mean, aes(x=c(55,55),  y=c(0.045, .04), label=paste0(survey, " mean length: ", format(round(Mean, digits=1), nsmall = 1) , " cm"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.12)) +
  scale_x_continuous(expand=c(0,5), breaks=seq(5,115,10)) +
  # facet_wrap(~survey, ncol=1) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

ggsave(file = paste0("results/", YEAR, "/all_len.png"), height = 4, width = 6, dpi=600)
# Alternative using ggridges

# Fishery lengths
lengths <- fsh.gren.len %>% 
  filter(gear == 1 | gear == 8, !is.na(region)) %>%
  # filter(nmfs_area > 609, nmfs_area < 651, !nmfs_area == 649) %>% 
  mutate(Gear = ifelse(gear == 1, "NPT", "HAL"))

mean_lengths <- lengths %>% 
  group_by(Gear, region) %>% 
  summarize(Mean = mean(length))

ggplot(lengths) + 
  geom_histogram(aes(x=length, y=after_stat(density), weight = frequency, fill = Gear),
                 position = 'identity', alpha=0.5, binwidth=1, col="black") +
  scale_fill_discrete(type = c('blue', 'red')) +
  xlab("Length (cm)") +
  ylab("Length composition by gear type") +
  # geom_text(data=mean_lengths, aes(x=c(85,85),  y=c(0.055, .05), label=paste0(Gear, " mean length: ", format(round(Mean, digits=1), nsmall = 1) , " cm"))) +
  scale_y_continuous(expand=c(0.00,0)) +
  scale_x_continuous(expand=c(0,5), breaks=seq(5,115,10)) +
  facet_wrap(~factor(region, levels=c('WGOA', 'CGOA', 'EGOA')), ncol = 1, scales = 'free_y') +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), 
        panel.grid.minor = element_blank()) 

ggsave(file = paste0("results/", YEAR, "/all_fish_region_len.png"), height = 8, width = 6, dpi=600)

# Fishery Catch
# catch <- fsh.sr.cat %>% 
#   filter(!agency_gear_code == "JIG") %>% # Use this if you want to remove some gear types (previous just had HAL and NPT)
#   group_by(year, agency_gear_code, region) %>% 
#   summarize(catch = sum(weight_posted))
# 
# ggplot(data.frame(catch), aes(as.integer(year), catch, col = agency_gear_code, group = agency_gear_code)) +
#   geom_line(linewidth = 1.2) +
#   geom_point(size = 2) +
#   scale_color_discrete(type = c('blue', 'red', 'orange', 'purple')) +
#   facet_grid(~factor(region, levels=c('WGOA', 'CGOA', 'EGOA'))) +
#   labs(y = "Catch (t)", x = "Year", col = "Gear") +
#   scale_x_continuous(breaks=seq(2010,2025,5)) +
#   scale_y_continuous(expand = c(0,0), limits = c(0, 375)) +
#   theme_bw() +
#   theme(axis.title=element_text(size=14), axis.text=element_text(size=12), panel.grid.minor = element_blank())
# 
# ggsave(file = paste0("results/", YEAR, "/Catch_TS.png"), height = 5, width = 10, dpi=600)

# Exploitation
# catch <- fsh.sr.cat %>% 
#   group_by(year, agency_gear_code, region) %>% 
#   summarize(catch = sum(weight_posted))
# 
# ggplot(catch, aes(year, catch, col = agency_gear_code)) +
#   geom_line() +
#   geom_point(size = 2) +
#   # scale_color_discrete(type = list(c("red", "blue"))) +
#   # geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
#   facet_grid(~factor(region, levels=c('WGOA', 'CGOA', 'EGOA'))) +
#   labs(y = "Catch (t)", x = "Year", col = "Gear") +
#   scale_x_continuous(breaks=seq(2010,2025,5)) +
#   scale_y_continuous(expand = c(0,0)) +
#   theme_bw() +
#   theme(axis.title=element_text(size=14), axis.text=element_text(size=12), panel.grid.minor = element_blank())
# 
# ggsave(file = paste0("results/", YEAR, "/Catch_TS.png"), height = 5, width = 10, dpi=600)

mean_lengths <- lengths %>% 
  group_by(year, Gear) %>% 
  summarize(Mean = mean(length))

ggplot(mean_lengths, aes(year, Mean, col = Gear)) + 
  geom_point()

ggplot()  + 
  geom_point(data = mean_lengths, aes(x = year, y = Mean, col = Gear)) + 
  geom_point(data = ll.means, aes(year, mean), size=4) +
  geom_point(data=means, aes(x=year, y=mean), size=4, pch=21) +
  ylab("Mean length (cm)") +
  scale_x_continuous(breaks=seq(1990,2020,5)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

