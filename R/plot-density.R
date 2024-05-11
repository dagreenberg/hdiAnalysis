##' Plot density function (smoothed) of MCMC recruitment for a given year, with tails shaded as specified
##'
##' @param dat_mcmc a tibble where each row represents an MCMC sample, and each
##'   column is a quantity which we want to summarise with credible intervals.
##' @param rec_intervals result of `create_intervals(dat_mcmc)`; is calculated
##'   if not supplied. May be worth supplying so it's not being repeatedly calculated.
##' @param year Which year of recruitment (age-0) to plot
##' @param type type of intervals: either `hdi` or `equal`
##' @return invisible
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot_recruitment_density()
##' plot_recruitment_density(year = 2021)
##' }
plot_density <- function(dat_mcmc = one_year_mcmc,
                                    dens_intervals = NULL,
                                     year = 2021,
                                     type = "hdi",
                                     x_lim = c(0, 40),  # default for 2010
                                     col_main = "red",
                                     col_tail = "blue",
                                     main_title = NULL,
                                     x_lab = NULL){
  if(!(type %in% c("equal", "hdi"))){
    stop("type needs to equal or hdi.")}

  if(is.null(dens_intervals)){
    dens_intervals <- calc_density(dat_mcmc)
  }

  dens <- dens_intervals$dens

  # Reorder Just use for title, maybe also low and high, actually prob not
  if(type == "equal"){
      if(is.null(main_title)) {
      main_title <- "Equal-tailed 95% interval"
    }
  } else { # type == "hdi"
    if(is.null(main_title)) {
      main_title <- "HDI 95% interval"
    }
  }

  if(is.null(x_lab)){
    x_lab <- paste0("Recruitment in ", year, " (billions of fish)")
  }

  # TODO change to x_interval_low etc.
  # low and high values of the interval for plotting, already calculated
  if(type == "equal"){
     interval_low <- dens_intervals$ints$`2.5`
     interval_high <- dens_intervals$ints$`97.5`
     y_interval_low <- dens_intervals$ints$y_low_equal_interp
     y_interval_high <- dens_intervals$ints$y_high_equal_interp
  } else { # type == "hdi"
     interval_low <- dens_intervals$ints$hdi_lower
     interval_high <- dens_intervals$ints$hdi_upper
     y_interval_low <- dens_intervals$ints$y_low_hdi_interp
     y_interval_high <- dens_intervals$ints$y_high_hdi_interp
  }

  plot(dens,
       xlab = x_lab,
       lwd = 2,
       xlim = x_lim,
       main = main_title)
# STILL need to think and CHECK EVERYTHING AGAIN

  # Full distribution
  polygon(dens,
          col = col_main,
          main = "")

  # Interval_Low tail
  polygon(c(dens$x[dens$x <= interval_low], interval_low, interval_low),
          c(dens$y[dens$x <= interval_low], y_interval_low, 0),
          col = col_tail,
          border = col_tail,
          main = "")

  # High tail
  polygon(c(interval_high, dens$x[dens$x >= interval_high], interval_high),
          c(y_interval_high, dens$y[dens$x >= interval_high], 0),
          col = col_tail,
          border = col_tail,
          main = "")

  # Make an if once figured out:
  abline(h = y_interval_low)



  # Prob remove this once fixed, just easier to debug with values in
#  return(list(dens = dens,
#              low = low,
#               high = high,
#               i_low = i_low,
#               y_low = y_low,
#               i_high = i_high,
#               y_high = y_high))
}
