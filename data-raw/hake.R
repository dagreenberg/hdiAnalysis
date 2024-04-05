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
                                       "R_1966":"R_2027")

names(hake_recruitment_mcmc) <- gsub(pattern = "R_",
                                     replacement = "",
                                     x = names(hake_recruitment_mcmc))

hake_recruitment_mcmc <- as_tibble(hake_recruitment_mcmc/1e6)    # Convert from thousands
                                                                 # to billions of fish
usethis::use_data(hake_recruitment_mcmc,
                  overwrite = TRUE)


# class(hake_recruitment_new) <- c("pacea_recruitment",
#                                 class(hake_recruitment_new))
# attr(hake_recruitment_new, "axis_name") <-
#  "Pacific Hake recruitment (billions of age-0 fish)"
