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

# Get bottom trawl survey biomass data
biom <- dbGetQuery(channel_akfin, 
                   "select    *
                from      afsc.race_biomassstratumaigoa
                where     species_code = '21230' and 
                          survey = 'AI' and 
                          year > 1983
                order by  year asc
                ") %>% 
  rename_all(tolower) %>% 
  filter(year < (YEAR + 1))

strata <- dbGetQuery(channel_akfin, 
                     "select    *
                from      afsc.race_goastrataaigoa
                where     survey = 'AI'
                ") %>% 
  rename_all(tolower)

biom2 <- left_join(biom, strata, by = c("stratum"))  

# Using all data like previous model
biomass <- biom2 %>% 
  filter(!summary_depth == 991) %>% 
  mutate(strata = ifelse(summary_depth == 992 | summary_depth == 993 | summary_depth == 994, "0-500 m",
                         ifelse(summary_depth == 995, "501-700 m",
                                ifelse(summary_depth == 996, "701-1000 m", NA)))) %>% 
  group_by(year, strata) %>% 
  summarize(n = n(), biomass = sum(stratum_biomass, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass)

# Same but with GAP products instead of race_
# Get bottom trawl survey biomass data
biom_new <- dbGetQuery(channel_akfin, 
                   "select    *
                from      gap_products.akfin_biomass
                where     species_code = '21230' and 
                          survey_definition_id = 52
                order by  year asc
                ") %>% 
  rename_all(tolower) %>% 
  filter(year < (YEAR + 1))


# Using all data like previous model
biomass_new <- biom_new %>% 
  filter(!area_id == 991) %>% 
  mutate(strata = ifelse(area_id == 992 | area_id == 993 | area_id == 994, "0-500 m",
                         ifelse(area_id == 995, "501-700 m",
                                ifelse(area_id == 996, "701-1000 m", NA)))) %>% 
  filter(!is.na(strata)) %>% 
  group_by(year, strata) %>% 
  summarize(n = n(), biomass = sum(biomass_mt, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

# Compare
compare <- biomass_new %>% 
  mutate(schema = "akfin.gap_products") %>% 
  select(schema, year, strata, n, biomass, cv) %>% 
  bind_rows(biomass %>% 
              mutate(schema = "akfin.afsc") %>% 
              select(schema, year, strata, n, biomass, cv)) 

ggplot(compare, aes(x = factor(year), y = biomass, fill = schema)) +
  geom_bar(stat = 'identity', position = position_dodge2(width = 0.5, preserve = "single", padding = 0)) +
  geom_errorbar(aes(ymin = biomass - cv * biomass, ymax = biomass + cv * biomass), 
                position = position_dodge2(width = 0.5, preserve = "single", padding = 0)) +
  facet_grid(~strata, scale = 'free_y') +
  labs(x = 'Year', y = 'Biomass (t)', fill = 'Schema') + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")

ggsave(file = paste0(out_path, "/GAP_compare.png"), height = 6, width = 12, units = 'in', dpi = 600)