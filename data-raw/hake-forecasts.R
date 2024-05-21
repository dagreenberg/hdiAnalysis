# Outputs for forecasts of relative spawning biomass from the hake stock assessment. Run code line-by-line
# and check plots. Full MCMC results are not already saved in an .rds for the
# hake assessment build, so I had to extract
# the relevant folders from the server. Just doing future catches of 264,000 (the 2023
# catch) and 350,000 (close to the average of the last 10 years, also shown in
# Fig j, prob what want to use). Doing this somewhat manually, based on `hake::load_forecasts()`.

# `mcmc_save()` could be used for the base model results as the full MCMC
# results were saved in the .rds for the base model. But this is not the case for the
# forecasts.

# Change assess_yr each year, the rest is automated.

load_all()
library(dplyr)

assess_yr <- 2024       # Year of the hake assessment; update each year

# Relative spawning biomass:
rel_2025 <- create_hake_forecast_mcmc(forecast_year_dir = 2025)
rel_2027 <- create_hake_forecast_mcmc(forecast_year_dir = 2027)
# expect_equal(rel_2025, rel_2027)   # As expected, fails because it's a new
#  MCMC simulation (though historic values match)

quants_2025 <- apply(rel_2025,
                     2,
                     quantile,
                     probs = c(0.05, 0.5, 0.95),
                     na.rm = TRUE)

# Using forecast-year-2025, this matches Table g for 2024, 2025, 2026, but not
# 2027.

## t(quants_2025) %>% round(digits = 2)
##        5%  50%  95%
## 2024 0.51 0.99 2.01
## 2025 0.49 1.01 2.13
## 2026 0.42 0.96 2.17
## 2027 0.32 0.73 1.83

# However, this next one matches Table g, so use this as covers all years.
quants_2027 <- apply(rel_2027,
                     2,
                     quantile,
                     probs = c(0.05, 0.5, 0.95),
                     na.rm = TRUE)

## t(quants_2027) %>% round(digits = 2)
##        5%  50%  95%
## 2024 0.51 0.99 2.01
## 2025 0.49 1.01 2.13
## 2026 0.42 0.96 2.17
## 2027 0.35 0.88 2.20

expect_equal(rel_2027$`2024`,
             hake_relative_biomass_mcmc$`2024`)   # So no need to resave 2024

hake_relative_biomass_mcmc_forecast <- dplyr::select(rel_2027,
                                                     -c("2024"))

usethis::use_data(hake_relative_biomass_mcmc_forecast,
                  overwrite = TRUE)
