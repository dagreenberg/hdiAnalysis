##' Create herring object to then save as data (five regions so best to
##' functionalise) for HDI.
##'
##' Used in `data-raw/herring/herring.R` to create data objects. Adapting from
##' pacea function.
##' @param assess_yr The year of the assessment
##' @author Andrew Edwards
##' @return List containing recruitment and biomass MCMC values, each as a
##' tibble with columns being years..
##' @examples
##' \dontrun{
##' }
##' @export
create_herring_object_hdi <- function(assess_yr,    # nocov start
                                      region,       # because need local files and
                                                    # not a user function anyway
                                      herring_dir = NULL){

  if(is.null(herring_dir)){
    herring_dir <- paste0(here::here(),
                          "/../pacea/data-raw/herring/herring-assessment-",
                          assess_yr,
                          "/")
  }

  load(file = paste0(herring_dir,
                     region,
                     "_aaa_gfiscam.RData"))
  # Recruitment
  raw_recruit_mcmc <- model$mcmccalcs$recr.dat

  # Convert from millions to billons to match SR Figures.
  recruitment <- tibble::as_tibble(raw_recruit_mcmc / 1000)

  raw_spawning_biomass_mcmc <- tibble::as_tibble(model$mcmccalcs$sbt.dat)
                                        # Checked HG 2023
                                        # matches Table 19 ETI

  return(list(recruitment = recruitment,
              spawning_biomass = raw_spawning_biomass_mcmc))
}                                    # nocovend
