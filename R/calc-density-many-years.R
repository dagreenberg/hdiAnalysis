##' For a tibble of MCMC samples as rows and some quantity in each column
##'  (assuming years, could generalise), calculate the equal and HDI intervals and the kernel
##'  densities for each column.
##'
##' TODO `n=1e5` is hardwired in here
##'
##' @param dat_mcmc
##' @param intervals
##' @param density if TRUE (the default) then use the density approach for the HDI
##'   calculation, rather than the `hdi()` default of just the sample values.
##' @param ... arguments to pass to `calc_density()`, that then get
##'   passed onto `create_intervals_density()` and then `density()`
##' @return list object of `res_all_years` which contains `year` plus
##'   `intervals` and `dens` for that year, plus `intervals_all_years` which
##'   contains all the intervals in one tibble with `year` as the first column.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' TODOone_year_mcmc <- dplyr::pull(hake_recruitment_mcmc, `2021`)
##' TODOcalc_density(one_year_mcmc)
##' }
calc_density_many_years <- function(dat_mcmc,
                                    ...){
  res_all_years <- list()
  intervals_all_years <- tibble()

  for(i in 1:ncol(dat_mcmc)){
    this_year = as.numeric(names(dat_mcmc))[i]

    this_year_mcmc <- pull(dat_mcmc,
                           as.character(this_year))    # Full MCMC samples for that year

    this_year_mcmc <- this_year_mcmc[!is.na(this_year_mcmc)]  # Remove NA's,
                                        # useful for sample size analysis

    res <- calc_density(this_year_mcmc,
                        n = 1e5,
                        ...)

    res_all_years[[i]] <- res
    res_all_years[[i]]$year <- this_year

    intervals_all_years <- rbind(
      intervals_all_years,
      cbind("year" = this_year,
            res$intervals))
  }
  intervals_all_years <- as_tibble(intervals_all_years)

  return(list(res_all_years = res_all_years,
              intervals_all_years = intervals_all_years))
}
