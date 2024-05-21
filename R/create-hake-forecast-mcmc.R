##' Extract relative spawning biomass calculations for forecasts for a given
##'  catch alternative.
##'
##' Need the full MCMC Stock Synthesis results in the correct folder, as the
##' assessment build does not automatically make .rds files that include MCMC
##' results for all model runs (it does for the base model). Not needed for
##' users, used to create `hake_relative_biomass_mcmc_forecast` in `data-raw/hake-forecasts.R`.
##'
##' @param catch_alternative which catch alternative (with index) to use, from
##'   Table g
##' @param forecast_year_dir which of the forecast years directories to use
##' @param model_path where to find the model Stock Synthesis results, gets set to
##'   Andy's directory if NULL
##' @param forecast_yrs the forecast years
##' @return tibble with each row an MCMC sample and characters as columns for
##'   the `forecast_yrs` to be returned
##' @export
##' @author Andrew Edwards, with some adapted from `hake::load_forecasts()`
##'   originally written by Chris Grandin
##' @examples
##' \dontrun{
##' # See data-raw/hake-forecasts.R; not needed by users
##' }
create_hake_forecast_mcmc <- function(catch_alternative = "06-350000",
                                      forecast_year_dir = "2027",
                                      model_path = NULL,
                                      forecast_yrs = 2024:2027){
  if(is.null(model_path)){
    model_path <- paste0(here::here(),
                         "/data-raw/hake-2024/02-version/01-base-models/")
  }

  mcmc_out <- r4ss::SSgetMCMC(paste0(model_path,
                                     "01-base/forecasts/forecast-year-",
                                     forecast_year_dir,
                                     "/",
                                     catch_alternative),
                              writecsv = FALSE,
                              verbose = FALSE)

  depl <- mcmc_out %>%
    select(grep("Bratio_", names(.)))
  names(depl) <- gsub("Bratio_", "", names(depl))

  depl_proj_cols <- depl %>%
    select(all_of(as.character(forecast_yrs))) %>%
    as_tibble()

  return(depl_proj_cols)
}
