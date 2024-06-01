# Outputs from the hake stock assessment. Run code line-by-line and check plots.

# hake-2024/hake_mcmc.rds contains a large tibble, with each row being an MCMC
# sample, and columns representing various things we might want to look
# at. Extracting some of the useful ones here to save as data objects. Will then
# come back to this to save more.

# Change assess_yr each year, the rest is automated.

# hake_recruitment will become the latest assessment results, and
#  hake_recruitment_2023 retains the 2023 assessment results, this is then ongoing for
#  each year. So hake_recruitment_<assess_yr> = hake_recruitment.
#  See ?hake for details.

load_all()
library(dplyr)

assess_yr <- 2024       # Year of the hake assessment; update each year

# The hake-2024/*.rds files is built automatically from (Andy or Chris Grandin)
#  running `mcmc_save()` in the hake-assessment repo after having just built the document.
# mcmc_save() automatically creates the hake-<assess_yr> directory here if it doesn't exist,
#  and puts the file into it, all named with assess_yr.

# Recruitment
hake_dir <- paste0(here::here(),
                   "/data-raw/hake-",
                   assess_yr,
                   "/")

# Full tibble of mcmc results
hake_mcmc <- readRDS(paste0(hake_dir,
                            "hake_mcmc.rds"))

# Check that we don't need to keep the Iteration column, can just use rownames
# if want to get back to the MCMC sample number

expect_equal(as.numeric(rownames(hake_mcmc)),
             dplyr::pull(hake_mcmc, "Iter"))

# These are identical
expect_equal(hake_mcmc["R_Virgin", ],
             hake_mcmc["R_Initial", ])



# Define objects similar to in pacea, but with _mcmc after

hake_recruitment_mcmc <- dplyr::select(hake_mcmc,
                                       "R_Virgin",
                                       "R_1966":"R_2024")

names(hake_recruitment_mcmc) <- gsub(pattern = "R_",
                                     replacement = "",
                                     x = names(hake_recruitment_mcmc))

hake_recruitment_mcmc <- as_tibble(hake_recruitment_mcmc/1e6)    # Convert from thousands
                                                                 # to billions of fish
usethis::use_data(hake_recruitment_mcmc,
                  overwrite = TRUE)

rec_2021 <- dplyr::pull(hake_recruitment_mcmc,
                        `2021`)

attr(rec_2021, "axis_name") <-
  "Recruitment (billions of age-0 fish)"

usethis::use_data(rec_2021,
                  overwrite = TRUE)

# class(hake_recruitment_new) <- c("pacea_recruitment",
#                                 class(hake_recruitment_new))


# Relative spawning biomass

hake_relative_biomass_mcmc <- dplyr::select(hake_mcmc,
                                            "Bratio_1967":"Bratio_2027")
                                            # There is no
                                            # Bratio_unfished, presumably since
                                            # should all be 1
hake_relative_biomass_mcmc

names(hake_relative_biomass_mcmc) <- gsub(pattern = "Bratio_",
                                          replacement = "",
                                          x = names(hake_relative_biomass_mcmc))

quantile(hake_relative_biomass_mcmc$`2024`, c(0.05, 0.50, 0.95))   # These match
                                                                   # Table g first row

quantile(hake_relative_biomass_mcmc$`2027`, c(0.05, 0.50, 0.95))   # These don't
                                                                   # match anything, so remove them
hake_relative_biomass_mcmc <- dplyr::select(hake_relative_biomass_mcmc,
                                            -c("2025", "2026", "2027"))

usethis::use_data(hake_relative_biomass_mcmc,
                  overwrite = TRUE)




# Trying to get experimental MCMC chains from server, as being used for Kelli's
# manuscript. Use exp for suffix for now.

hake_dir_exp <- paste0(here::here(),
                       "/data-raw/hake-",
                       assess_yr,
                       "/02-version-05-test-models/")

hake_14_long <- readRDS(paste0(hake_dir_exp,
                            "14-long-base.rds"))
# Contains all MCMC results and maybe more, as a list object.
# There's an hake.Rdata file in mcmc/ on the server but I don't think
#    that's useful, has things like number of chains etc, don't think it has results.

# Using some from hake-assessment-2024::mcmc_save()
dat <- as_tibble(hake_14_long$mcmc)

keep <- select(dat,
               "Iter":"SR_BH_steep",
               "SSB_Virgin":"Recr_2027",
               "Bratio_1967":"Recr_unfished",
               "B_MSY/SSB_unfished":"ForeCatch_2027",
               "Dyn_Bzero_Virg":"ln(SSB)_2023")

names(keep) = gsub(pattern = "SSB_",
                   replacement = "B_",
                   x = names(keep))

names(keep) = gsub(pattern = "Recr_",
                   replacement = "R_",
                   x = names(keep))

keep

# Copying from above to just have recruitment, need 14_long
hake_recruitment_mcmc_14_long <- dplyr::select(keep,
                                               "R_Virgin",
                                               "R_1966":"R_2024")

names(hake_recruitment_mcmc_14_long) <- gsub(pattern = "R_",
                                             replacement = "",
                                             x = names(hake_recruitment_mcmc_14_long))

hake_recruitment_mcmc_14_long <- as_tibble(hake_recruitment_mcmc_14_long/1e6)    # Convert from thousands
                                                                 # to billions of fish

usethis::use_data(hake_recruitment_mcmc_14_long,
                  overwrite = TRUE)
