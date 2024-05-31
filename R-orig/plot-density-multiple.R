##' Plot density function (smoothed) of MCMC recruitment for multiple years, with tails shaded as specified
##'
##' Calls `plot_density()` once for each specified year of MCMC samples and
##' results. Specify `paf(mfrow = c(4,3))` or whatever first of all before calling.
##'
##' @param dat_mcmc_years a numeric tibble representing MCMC samples with
##'   columns for each year and rows for each MCMC sample.
##' @param dens_intervals_years list, result `calc_density_many_years()`
##' @param years numeric vector of which years to plot (could generalise to not be only years)
##' @param type type of intervals: either `hdi` or `equal`
##' @param ... arguments to be passed onto `plot_density()` and then onto others
##' @return invisible
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot_recruitment_density() TODO
##' plot_recruitment_density(year = 2021)
##' }
plot_density_multiple <- function(dat_mcmc_years,
                         dens_intervals_years,
                         years,
                         x_lab = "Recruitment (billions of fish)",
                         ...){
                         # Think ... will work
                         #type = "hdi",
                         #x_lim = c(0, 40),  # default for 2010
                         #col_main = "red",
                         #col_tail = "blue",
                         #main_title = NULL,
                         #x_lab = NULL){

  years_in_results <- dens_intervals_years$intervals_all_years$year # should
                                        # automatically match the one for each list object


  for(i in 1:length(years)){
    dat_mcmc_this_year <- pull(dat_mcmc_years,
                               years[i])
    results_index <- which(years_in_results == years[i])

    plot_density(dat_mcmc = dat_mcmc_this_year,
                 dens_intervals =
                   dens_intervals_years$res_all_years[[results_index]],
                 x_lab = x_lab,
                 main_title = years[i],
                 ...)
  }
}
