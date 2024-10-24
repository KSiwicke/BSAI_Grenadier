# GOA grenadier biomass estimation using the bottom trawl and longline survey indices

# Model naming conventions:
# M24.0 - slope trawl only, ends in 2016
# M24.1 - add LLS to trawl, extends data to 2023
# M24.2 - Just LLS, assumes biomass to RPW is 1:1, which may be ok since the scaling parameter in M24.1 ~ 1

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
dat_path <- paste0("data/ebs_", YEAR); dir.create(dat_path)
out_path <- paste0("results/ebs_", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 15) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# Read data ----
source("r/ebs_biom_data_pull_GAP_24.r")

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
combo <- cpue_dat |> rename(index = cpue) |> mutate(Survey = "LLS") |> 
  bind_rows(biomass_dat |> rename(index = biomass) |> mutate(Survey = "BTS"))

ggplot(combo, aes(year, index / 100000, col = Survey)) +
  geom_point() +
  geom_errorbar(aes(ymin = (index - index*cv) / 100000, ymax = (index + index*cv) / 100000)) +
  labs(x = "Year", y = "Index (hundred thousand)") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 8))

ggsave(filename = paste0(out_path, '/EBS_index_raw.png'), bg = 'white',
       dpi = 300, units = 'in', height = 4, width = 8)

# Model 24.0 - Slope trawl survey only ----
input <- prepare_rema_input(model_name = 'Model 24.0 - BTS Only',
                            multi_survey = 0,
                            biomass_dat = biomass_dat,
                            sum_cpue_index = TRUE,
                            start_year = 1997,
                            end_year = YEAR + 2)
                            
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
       dpi = 300, bg = 'white', units = 'in', height = 4, width = 6)

# Model 24.1, add longline survey index ----
input <- prepare_rema_input(model_name = 'Model 24.1 BTS + LLS',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            start_year = 1997,
                            end_year = YEAR + 2)

m24.1 <- fit_rema(input)
out24.1 <- tidy_rema(m24.1)
out24.1$parameter_estimates 

# Compare M24.0 and 24.1 ----
compare <- compare_rema_models(rema_models = list(m24.0, m24.1))
lls <- compare_rema_models(rema_models = list(m24.1))

cowplot::plot_grid(compare$plots$biomass_by_strata +
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
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 9)

# Model 24.2, longline survey only ----
# Assumes RPW equal to biomass, which is reasonable here
input <- prepare_rema_input(model_name = 'Model 24.2 - LLS Only',
                            multi_survey = 0,
                            biomass_dat = cpue_dat |> rename(biomass = cpue),
                            sum_cpue_index = TRUE,
                            start_year = 1997,
                            end_year = YEAR + 2)

m24.2 <- fit_rema(input)
out24.2 <- tidy_rema(m24.2)
out24.2$parameter_estimates 

# Compare M24.0, 24.1, and 24.2 ----
compare <- compare_rema_models(rema_models = list(m24.0, m24.1, m24.2))

compare$plots$biomass_by_strata +
  theme(legend.position = 'top') +
  geom_line() +
  labs(x = NULL, y = NULL, subtitle = 'Biomass (t)',
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  scale_fill_discrete(type = c('#440154FF', '#FDE725FF', '#22A884FF')) +
  scale_color_discrete(type = c('#440154FF', '#FDE725FF', '#22A884FF'))

ggsave(filename = paste0(out_path, '/all_mod_totalbiomass.png'),
       dpi = 300, bg = 'white', units = 'in', height = 5, width = 9)

full_sumtable <- compare$output$total_predicted_biomass %>%
  filter(year == YEAR + 1) %>%
  mutate(natmat = 0.078,
         OFL = natmat * pred,
         maxABC = 0.75 * natmat * pred,
         ABC = maxABC)

sumtable_std <- full_sumtable %>%
  distinct(model_name, year, biomass = pred, OFL, maxABC) %>%
  select(model_name, year, biomass, OFL, maxABC) %>%
  write_csv(paste0(out_path, '/abc_ofl_summary.csv'))

# Pull length data and make figures
# source("code/Length_figures.r")

# Survey comparisons with figures
# source("code/survey_comparison.r")
