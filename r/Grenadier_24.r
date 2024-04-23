# GOA grenadier biomass estimation using the bottom trawl and longline survey indices

# Model naming conventions:
# Model 1 = m1 is biomass only for comparison only
# Model 19* = m19s: 1990-pres. corrected version of status quo model
# Model 19* w/ 1984/97 = m19b: 1984-pres. corrected version of status quo model
# Model 19*X = m19sX: 1990-pres. corrected version of status quo model, downweights LLS to 0.2 just for reference...this really improves model!
# Model 23.1  = m23.1 is M19* but changes weight of LLS to 1.0 and other
# from 23.1, then m23.2 (xtra BTS OE), m23.3 (xtra LLS OE), and m23.4 (both xtra OE)

# Set up ----

# assessment year
YEAR <- 2023

# Consider whether the rema package needs to be 'updated' - will need to update to get new extra_cv fxns
# install.packages("devtools")
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE) #, force = TRUE

libs <- c('rema', 'readr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# folder set up
dat_path <- paste0("data/", YEAR); dir.create(dat_path)
out_path <- paste0("results/", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 15) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# Read data ----
source("code/biom_data_pull_GAP_24.r")

# bottom trawl survey
biomass_dat <- model_dat$biomass_dat
biomass_dat %>% 
  write_csv(paste0(dat_path, "/goa_gren_biomass_", YEAR, ".csv"))
biomass_dat %>%
  tidyr::expand(year = min(biomass_dat$year):(YEAR),
                strata) %>%
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
  write_csv(paste0(dat_path, "/goa_gren_rpw_", YEAR, ".csv"))
 
# Model 24.1 3 areas, 3 depth strata for BTS, single process error ----
input <- prepare_rema_input(model_name = 'Model 24.1 Single PE',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            start_year = 1990,
                            end_year = YEAR + 2,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1, 1, 1, 1, 1, 1)),
                            q_options = list(
                              pointer_biomass_cpue_strata = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                              pointer_q_cpue = c(1, 2, 3)))
                            
                            
                           
m24.1 <- fit_rema(input)
out24.1 <- tidy_rema(m24.1)
out24.1$parameter_estimates 

# Model 24.2, same as 24.1 but with three different process errors by region----
input <- prepare_rema_input(model_name = 'Model 24.2 (3-area PE)',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            # start at 1990 instead of 1984
                            start_year = 1990,
                            end_year = YEAR + 2,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1, 2, 2, 2, 3, 3, 3)),
                            q_options = list(
                              pointer_biomass_cpue_strata = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                              pointer_q_cpue = c(1, 2, 3)))
m24.2 <- fit_rema(input)
out24.2 <- tidy_rema(m24.2)
out24.2$parameter_estimates

# Model 24.3, same as 24.1 but with three different process errors by depths ----
input <- prepare_rema_input(model_name = 'Model 24.3 (3-depth PE)',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            # start at 1990 instead of 1984
                            start_year = 1990,
                            end_year = YEAR + 2,
                            PE_options = list(pointer_PE_biomass = c(1, 2, 3, 1, 2, 3, 1, 2, 3)),
                            q_options = list(
                              pointer_biomass_cpue_strata = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                              pointer_q_cpue = c(1, 2, 3)))
m24.3 <- fit_rema(input)
out24.3 <- tidy_rema(m24.3)
out24.3$parameter_estimates

# Compare M19* with and without 1984/87 ----
compare <- compare_rema_models(rema_models = list(m24.1, m24.2, m24.3))

cowplot::plot_grid(compare$plots$biomass_by_strata +
                     theme(legend.position = 'none') +
                     geom_line() +
                     labs(x = NULL, y = NULL, subtitle = 'Trawl survey biomass (t)',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF', '#2A788EFF', '#FDE725FF')) +
                     coord_cartesian(ylim=c(0, 500000)) +
                     scale_color_discrete(type = c('#440154FF', '#2A788EFF', '#FDE725FF')),
                   compare$plots$cpue_by_strata  +
                     facet_wrap(~strata, ncol = 1)  +
                     geom_line() +
                     labs(x = NULL, y = NULL, subtitle = 'Longline survey RPW',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF', '#2A788EFF', '#FDE725FF')) +
                     # coord_cartesian(ylim=c(0, 60000)) +
                     scale_color_discrete(type = c('#440154FF', '#2A788EFF', '#FDE725FF')),
                   ncol = 2,
                   rel_widths = c(1.5, 1))

# ggsave(filename = paste0(out_path, '/M22_3pe_vs_1pe_fits.png'),
#        dpi = 400, bg = 'white', units = 'in', height = 9, width = 14)

compare$plots$total_predicted_biomass +
  labs(subtitle = 'Total predicted biomass (t)',
       fill = NULL, colour = NULL) +
  scale_fill_discrete(type = c('#440154FF', '#2A788EFF', '#FDE725FF')) +
  # coord_cartesian(ylim=c(0, 60000)) +
  scale_color_discrete(type = c('#440154FF', '#2A788EFF', '#FDE725FF'))

# ggsave(filename = paste0(out_path, '/M18b_M22b_totalbiomass.png'),
#        dpi = 400, bg = 'white', units = 'in', height = 3.5, width = 8)

# Easy to assert using a single process error

# Model 24.1 3 areas, 3 depth strata for BTS, single process error ----
input <- prepare_rema_input(model_name = 'Model 24.4 Single PE with extra OE',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            start_year = 1990,
                            end_year = YEAR + 2,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1, 1, 1, 1, 1, 1)),
                            q_options = list(
                              pointer_biomass_cpue_strata = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                              pointer_q_cpue = c(1, 2, 3)),
                            extra_biomass_cv = list(assumption = 'extra_cv'),
                            extra_cpue_cv = list(assumption = 'extra_cv'))

m24.4 <- fit_rema(input)
out24.4 <- tidy_rema(m24.4)
out24.4$parameter_estimates 

compare <- compare_rema_models(rema_models = list(m24.1, m24.4))

cowplot::plot_grid(compare$plots$biomass_by_strata +
                     theme(legend.position = 'none') +
                     geom_line() +
                     labs(x = NULL, y = NULL, subtitle = 'Trawl survey biomass (t)',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF', '#7AD151FF')) +
                     coord_cartesian(ylim=c(0, 500000)) +
                     scale_color_discrete(type = c('#440154FF', '#7AD151FF')),
                   compare$plots$cpue_by_strata  +
                     facet_wrap(~strata, ncol = 1)  +
                     geom_line() +
                     labs(x = NULL, y = NULL, subtitle = 'Longline survey RPW',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF', '#7AD151FF')) +
                     # coord_cartesian(ylim=c(0, 60000)) +
                     scale_color_discrete(type = c('#440154FF', '#7AD151FF')),
                   ncol = 2,
                   rel_widths = c(1.5, 1))

# ggsave(filename = paste0(out_path, '/M22_3pe_vs_1pe_fits.png'),
#        dpi = 400, bg = 'white', units = 'in', height = 9, width = 14)

compare$plots$total_predicted_biomass +
  labs(subtitle = 'Total predicted biomass (t)',
       fill = NULL, colour = NULL) +
  scale_fill_discrete(type = c('#440154FF', '#7AD151FF')) +
  # coord_cartesian(ylim=c(0, 60000)) +
  scale_color_discrete(type = c('#440154FF', '#7AD151FF'))

# ggsave(filename = paste0(out_path, '/M18b_M22b_totalbiomass.png'),
#        dpi = 400, bg = 'white', units = 'in', height = 3.5, width = 8)

plot_extra_cv(out24.4)$biomass_by_strata +
  labs(x = NULL, y = 'Biomass (t)') +
  facet_wrap(~factor(strata, levels=c('WGOA', 'CGOA', 'EGOA')), ncol = 3)

ggsave(filename = paste0(out_path, '/m23.2_xtra_bts_oe.png'),
       dpi = 600, bg = 'white', units = 'in', height = 5, width = 12)

plot_extra_cv(out24.4$cpue_by_strata) +
  coord_cartesian(ylim=c(0, 55000)) +
  labs(x = NULL, y = 'Relative Population Weights') +
  facet_wrap(~factor(strata, levels=c('WGOA', 'CGOA', 'EGOA')), ncol = 3)

ggsave(filename = paste0(out_path, '/m23.2_xtra_lls_oe.png'),
       dpi = 600, bg = 'white', units = 'in', height = 5, width = 12)

params <- bind_rows(out19b$parameter_estimates, out19s$parameter_estimates, 
                   out23.1$parameter_estimates, out23.2$parameter_estimates, 
                   out23.3$parameter_estimates, out23.4$parameter_estimates) %>% 
  write_csv(paste0(out_path, '/parameter_values.csv'))
# The smoothest line is the one with only including an extra OE on the LLS
# but that was the worst by AIC...this might make the more sense to use

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

full_sumtable_std <- appo_std %>%
  filter(year == YEAR + 1) %>%
  mutate(natmat = 0.03,
         OFL = natmat * total_biomass,
         maxABC = 0.75 * natmat * total_biomass,
         ABC = maxABC)

sumtable_std <- full_sumtable_std %>%
  distinct(model_name, year, biomass = total_biomass, OFL, maxABC) %>%
  select(model_name, year, biomass, OFL, maxABC) %>%
  write_csv(paste0(out_path, '/abc_ofl_summary.csv'))

appo_lls <- compare$output$cpue_by_strata %>%
  mutate(strata = ifelse(grepl('CGOA', strata), 'CGOA',
                         ifelse(grepl('EGOA', strata), 'EGOA',
                                'WGOA'))) %>%
  group_by(model_name, strata, year) %>%
  summarize(stratum_biomass = sum(pred)) %>%
  group_by(model_name, year) %>%
  mutate(total_biomass = sum(stratum_biomass)) %>%
  ungroup() %>%
  mutate(proportion_lls = stratum_biomass / total_biomass) %>%
  mutate(strata = factor(strata, labels = c('EGOA', 'CGOA', 'WGOA'), levels = c('EGOA', 'CGOA', 'WGOA'), ordered = TRUE)) %>%
  arrange(year, strata)

appo_lls %>%
  pivot_wider(id_cols = c(strata, year), names_from = model_name, values_from = proportion_lls) %>%
  write_csv(paste0(out_path, '/m19s_m23.3_apportionment_lls.csv'))

ggplot(appo_lls %>% filter(model_name == 'Model 23.3 (23.1 w/ extra LLS OE)'), aes(year, proportion_lls)) + 
  geom_col(aes(fill = strata)) + 
  facet_wrap(~model_name) +
  coord_flip() +
  labs(x = NULL, y = 'Proportion', fill = 'Region')

ggsave(filename = paste0(out_path, '/m19s_m23.3_appo_lls.png'),
       dpi = 600, bg = 'white', units = 'in', height = 8.5, width = 10)

# ggplot(appo_lls %>% filter(model_name == 'Model 19*'), aes(year, proportion)) + geom_col(aes(fill = strata)) + coord_flip()
# ggplot(appo_lls %>% filter(model_name == 'Model 23.3 (23.1 w/ extra LLS OE)'), aes(year, proportion)) + geom_col(aes(fill = strata)) + coord_flip()

full_sumtable_lls <- appo_lls %>%
  filter(year == YEAR + 1) %>%
  mutate(natmat = 0.03,
         OFL = natmat * total_biomass,
         maxABC = 0.75 * natmat * total_biomass,
         ABC = maxABC)

sumtable_lls <- full_sumtable_lls %>%
  distinct(model_name, year, biomass = total_biomass, OFL, maxABC) %>%
  select(model_name, year, biomass, OFL, maxABC) %>%
  write_csv(paste0(out_path, '/abc_ofl_summary.csv'))

# Combine for average proportions...
appo_combo <- left_join(appo_std, appo_lls %>% select(model_name, strata, year, proportion_lls)) %>% 
  mutate(proportion = (proportion_std + proportion_lls) / 2)

ggplot(appo_combo %>% filter(model_name == 'Model 23.3 (23.1 w/ extra LLS OE)'), aes(year, proportion)) + 
  geom_col(aes(fill = strata)) + 
  facet_wrap(~model_name) +
  coord_flip() +
  labs(x = NULL, y = 'Proportion', fill = 'Region')

ggsave(filename = paste0(out_path, '/m19s_m23.3_appo_combo.png'),
       dpi = 600, bg = 'white', units = 'in', height = 8.5, width = 10)

full_sumtable_combo <- appo_combo %>%
  filter(year == YEAR + 1) %>%
  mutate(natmat = 0.03,
         OFL = natmat * total_biomass,
         maxABC = 0.75 * natmat * total_biomass,
         ABC = maxABC)

sumtable_combo <- full_sumtable_combo %>%
  distinct(model_name, year, biomass = total_biomass, OFL, maxABC) %>%
  select(model_name, year, biomass, OFL, maxABC) %>%
  write_csv(paste0(out_path, '/abc_ofl_summary.csv'))

cowplot::plot_grid(ggplot(appo_std %>% filter(model_name == 'Model 23.3 (23.1 w/ extra LLS OE)'), aes(year, proportion_std)) + 
                     geom_col(aes(fill = strata)) + 
                     facet_wrap(~model_name) +
                     coord_flip() +
                     labs(x = NULL, y = 'Proportion', fill = 'Region'),
                   ggplot(appo_combo %>% filter(model_name == 'Model 23.3 (23.1 w/ extra LLS OE)'), aes(year, proportion)) + 
                     geom_col(aes(fill = strata)) + 
                     facet_wrap(~model_name) +
                     coord_flip() +
                     labs(x = NULL, y = 'Proportion', fill = 'Region'),
                   ncol = 1)

ggsave(file = paste0("results/", YEAR, "/appo_fig.png"), height = 7, width = 9, dpi=600)

# # percent changes -----
# biomass_dat %>% filter(year %in% c(2019,2021)) %>%
#   mutate(strata = ifelse(grepl('CGOA', strata), 'CGOA',
#                          ifelse(grepl('EGOA', strata), 'EGOA',
#                                 'WGOA'))) %>%
#   group_by(year, strata) %>%
#   summarise(biomass = sum(biomass, na.rm = T)) %>%
#   pivot_wider(id_cols = c(strata), names_from = year, values_from = biomass) %>%
#   mutate(percent_change = (`2021`-`2019`)/`2019`)
# 
# cpue_dat %>% filter(year %in% c(2021, 2022)) %>%
#   pivot_wider(id_cols = c(strata), names_from = year, values_from = cpue) %>%
#   mutate(percent_change = (`2022`-`2021`)/`2021`)
# 
# old <- out19s$total_predicted_biomass %>% filter(year == 2022) %>% pull(pred)
# new <- out23$total_predicted_biomass %>% filter(year == 2022) %>% pull(pred)
# (new-old)/old
# 
# # Table for predicted biomass by strata with total LCI/UCI
# strata_table <- compare$output$biomass_by_strata %>%
#   filter(model_name == "Model 23") %>% 
#   mutate(strata = ifelse(grepl('CGOA', strata), 'CGOA',
#                          ifelse(grepl('EGOA', strata), 'EGOA',
#                                 'WGOA'))) %>%
#   group_by(year, strata) %>%
#   summarise(biomass = sum(pred, na.rm = T), lci = sum(pred, na.rm = T), uci = sum(pred, na.rm = T) ) %>%
#   pivot_wider(id_cols = c(year), names_from = strata, values_from = biomass) %>% 
#   mutate(WGOA = round(WGOA, 0), CGOA = round(CGOA, 0), EGOA = round(EGOA, 0)) %>% 
#   write.csv(paste0(out_path, '/M23_Table_strata_biomass_pred.csv'))
# 
# goa_table <- compare$output$total_predicted_biomass %>% 
#   filter(model_name == "Model 23") %>% 
#   select(year, pred, pred_lci, pred_uci) %>% 
#   mutate(GOA_Total = round(pred, 0), LCI = round(pred_lci, 0), UCI = round(pred_uci, 0)) %>% 
#   write.csv(paste0(out_path, '/M23_Table_goa_biomass_pred.csv'))

# Pull length data and make figures
source("code/Length_figures.r")

# Survey comparisons with figures
# source("code/survey_comparison.r")
