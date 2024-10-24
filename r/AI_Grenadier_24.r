# GOA grenadier biomass estimation using the bottom trawl and longline survey indices

# Model naming conventions:
# M24.0 - slope trawl only, ends in 2016
# M24.1 - add LLS to trawl, extends data to 2023

# Set up ----

# assessment year
YEAR <- 2024

# Consider whether the rema package needs to be 'updated' - will need to update to get new extra_cv fxns
# install.packages("devtools")
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE) #, force = TRUE

libs <- c('rema', 'readr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# folder set up
dat_path <- paste0("data/ai_", YEAR); dir.create(dat_path)
out_path <- paste0("results/ai_", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 15) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# Read data ----
source("r/ai_biom_data_pull_GAP_24.r")

# bottom trawl survey
biomass_dat <- model_dat$biomass_dat
biomass_dat %>% 
  write_csv(paste0(dat_path, "/ebs_gren_biomass_", YEAR, ".csv"))

biomass_dat %>%
  tidyr::expand(year = min(biomass_dat$year):(YEAR), strata) %>%
  left_join(biomass_dat %>%
              mutate(value = ifelse(is.na(biomass), NA,
                                    paste0(prettyNum(round(biomass, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) %>%
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) %>%
  arrange(year) %>%
  write_csv(paste0(out_path, '/biomass_data_wide.csv'))

# longline survey rpws
cpue_dat <- model_dat$cpue_dat 
cpue_dat %>% 
  write_csv(paste0(dat_path, "/ebs_gren_rpw_", YEAR, ".csv"))

cpue_dat |> 
  tidyr::expand(year = min(cpue_dat$year):(YEAR), strata) %>%
  left_join(cpue_dat %>%
              mutate(value = ifelse(is.na(cpue), NA,
                                    paste0(prettyNum(round(cpue, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) %>%
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) %>%
  arrange(year) %>%
  write_csv(paste0(out_path, '/cpue_data_wide.csv'))

# Plot raw data by from both surveys
# combo <- cpue_dat |> rename(index = cpue) |> mutate(Survey = "LLS") |> 
#   bind_rows(biomass_dat |> rename(index = biomass) |> mutate(Survey = "BTS"))
# 
# ggplot(combo, aes(year, index / 100000, col = Survey)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = (index - index*cv) / 100000, ymax = (index + index*cv) / 100000)) +
#   labs(x = "Year", y = "Index (hundred thousand)") +
#   scale_y_continuous(expand = c(0,0), limits = c(0, 8))
# 
# ggsave(filename = paste0(out_path, '/EBS_index_raw.png'), bg = 'white',
#        dpi = 300, units = 'in', height = 4, width = 8)

# Model 24.0 - Slope trawl survey only ----
input <- prepare_rema_input(model_name = 'Model 24.0 - Trawl Only',
                            multi_survey = 0,
                            biomass_dat = cpue_dat |> mutate(biomass = cpue / 4),
                            sum_cpue_index = TRUE,
                            start_year = 1996,
                            end_year = YEAR + 2,
                            extra_biomass_cv = list(assumption = 'extra_cv'))
                            
m24.0 <- fit_rema(input)
out24.0 <- tidy_rema(m24.0)
out24.0$parameter_estimates 

compare <- compare_rema_models(rema_models = list(m24.0))

compare$plots$biomass_by_strata +
  theme(legend.position = 'top') +
  geom_line() +
  labs(x = NULL, y = NULL, subtitle = 'Trawl survey biomass (t)',
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_fill_discrete(type = c('#440154FF')) +
  scale_color_discrete(type = c('#440154FF'))

ggsave(filename = paste0(out_path, '/BTS_REMA_totalbiomass.png'),
       dpi = 400, bg = 'white', units = 'in', height = 4, width = 6)

# # Model 24.1 Trawl Survey only with extra Obs Error ----
# input <- prepare_rema_input(model_name = 'Model 24.1 - Trawl Xtra Err',
#                             multi_survey = 0,
#                             biomass_dat = biomass_dat,
#                             sum_cpue_index = TRUE,
#                             start_year = 1997,
#                             end_year = YEAR + 2,
#                             extra_biomass_cv = list(assumption = 'extra_cv'))
# 
# m24.1 <- fit_rema(input)
# out24.1 <- tidy_rema(m24.1)
# out24.1$parameter_estimates
# 
# # Compare M24.0 and M24.1 ----
# compare <- compare_rema_models(rema_models = list(m24.0, m24.1))
# 
# compare$plots$total_predicted_biomass +
#   labs(subtitle = 'Total predicted biomass (t)',
#        fill = NULL, colour = NULL) +
#   scale_fill_discrete(type = c('#440154FF', '#2A788EFF', '#FDE725FF')) +
#   scale_color_discrete(type = c('#440154FF', '#2A788EFF', '#FDE725FF'))


# Model 24.2, add longline survey index ----
input <- prepare_rema_input(model_name = 'Model 24.1',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            start_year = 1997,
                            end_year = YEAR + 2)

m24.1 <- fit_rema(input)
out24.1 <- tidy_rema(m24.1)
out24.1$parameter_estimates 

# Compare M19* with and without 1984/87 ----
compare <- compare_rema_models(rema_models = list(m24.1))
lls <- compare_rema_models(rema_models = list(m24.1))

cowplot::plot_grid(compare_2$plots$biomass_by_strata +
                     theme(legend.position = 'top') +
                     geom_line() +
                     labs(x = NULL, y = NULL, subtitle = 'Trawl survey biomass (t)',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF', '#FDE725FF')) +
                     scale_color_discrete(type = c('#440154FF', '#FDE725FF')), 
                   lls$plots$cpue_by_strata +
                     theme(legend.position = 'none') +
                     geom_line() +
                     labs(x = NULL, y = NULL, subtitle = 'Longline survey RPW',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#FDE725FF')) +
                     
                     scale_color_discrete(type = c('#FDE725FF')),
                   ncol = 2, align = "h")

ggsave(filename = paste0(out_path, '/add_lls_totalbiomass.png'),
       dpi = 400, bg = 'white', units = 'in', height = 5, width = 9)

# apportionment ----
compare <- compare_rema_models(rema_models = list(m19s, m23.1, m23.2, m23.3))

appo_std <- compare$output$biomass_by_strata %>%
  mutate(strata = ifelse(grepl('CGOA', strata), 'CGOA',
                         ifelse(grepl('EGOA', strata), 'EGOA',
                                'WGOA'))) %>%
  group_by(model_name, strata, year) %>%
  summarize(stratum_biomass = sum(pred)) %>%
  group_by(model_name, year) %>%
  mutate(total_biomass = sum(stratum_biomass)) %>%
  ungroup() %>%
  mutate(proportion_std = stratum_biomass / total_biomass) %>%
  mutate(strata = factor(strata, labels = c('EGOA', 'CGOA', 'WGOA'), levels = c('EGOA', 'CGOA', 'WGOA'), ordered = TRUE)) %>%
  arrange(year, strata)

appo_std %>%
  pivot_wider(id_cols = c(strata, year), names_from = model_name, values_from = proportion_std) %>%
  write_csv(paste0(out_path, '/m19s_m23.1_m23.2_m23.3_apportionment_standard.csv'))

ggplot(appo_std %>% filter(model_name == 'Model 23.3 (23.1 w/ extra LLS OE)'), aes(year, proportion_std)) + 
  geom_col(aes(fill = strata)) + 
  facet_wrap(~model_name) +
  coord_flip() +
  labs(x = NULL, y = 'Proportion', fill = 'Region')

ggsave(filename = paste0(out_path, '/m19s_m23.3_appo_std.png'),
       dpi = 600, bg = 'white', units = 'in', height = 8.5, width = 10)

# ggplot(appo_std %>% filter(model_name == 'Model 23.3 (23.1 w/ extra LLS OE)'), aes(year, proportion)) + geom_col(aes(fill = strata)) + coord_flip()

full_sumtable <- compare$output$total_predicted_biomass %>%
  filter(year == YEAR + 1) %>%
  mutate(natmat = 0.078,
         OFL = natmat * pred,
         maxABC = 0.75 * natmat * pred,
         ABC = maxABC)

sumtable_std <- full_sumtable_std %>%
  distinct(model_name, year, biomass = total_biomass, OFL, maxABC) %>%
  select(model_name, year, biomass, OFL, maxABC) %>%
  write_csv(paste0(out_path, '/abc_ofl_summary.csv'))

# Pull length data and make figures
source("code/Length_figures.r")

# Survey comparisons with figures
# source("code/survey_comparison.r")
