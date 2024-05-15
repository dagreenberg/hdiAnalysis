##' Plot density function (smoothed) of MCMC recruitment for a given year, with tails shaded as specified
##'
##' @param dat_mcmc a numeric vector representing an MCMC sample.
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
plot_density_fig1 <- function(dat_mcmc = one_year_mcmc,
                         dens_intervals = NULL,
                         year = 2021,
                         type = "hdi",
                         x_lim = c(0, 50),  # default for 2010
                         col_main = NA,
                         col_tail_low = rgb(0, 114, 178, maxColorValue = 255), # Blue from color blind pallet (Okabe-Ito)
                         col_tail_high =rgb(0, 114, 178, maxColorValue = 255), # Sky blue from color blind pallet (Okabe-Ito)
                         col_included = rgb(204, 121, 167, maxColorValue = 255), # Reddish purple from color blind pallet (Okabe-Ito) #rgb(230, 159, 0, maxColorValue = 255), # Orange from color blind pallet (Okabe-Ito)
                         main_title = NULL,
                         x_lab = NULL){
  if(!(type %in% c("equal", "hdi"))){
    stop("type needs to equal or hdi.")}

  if(is.null(dens_intervals)){
    dens_intervals <- calc_density(dat_mcmc, n=1e5)
  }

  dens <- dens_intervals$density

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
     interval_low <- dens_intervals$intervals$`2.5`
     interval_high <- dens_intervals$intervals$`97.5`
     y_interval_low <- dens_intervals$intervals$y_low_equal_interp
     y_interval_high <- dens_intervals$intervals$y_high_equal_interp
  } else { # type == "hdi"
     interval_low <- dens_intervals$intervals$hdi_lower
     interval_high <- dens_intervals$intervals$hdi_upper
     y_interval_low <- dens_intervals$intervals$y_low_hdi_interp
     y_interval_high <- dens_intervals$intervals$y_high_hdi_interp
  }

  plot(dens,
       xlab = x_lab,
       lwd = 1.2,
       xlim = x_lim,
       main = main_title,
       las=1)

  # legend(x = "topright", legend =c("1  lower tail", "2  upper tail", "3  included (but as probable as lower tail)"), 
  #        col = c(col_tail_low, col_tail_high, col_included), pch = 19, bty = "n", pt.cex = 2.2, x.intersp = -0.32)
  legend(x = "topright", legend =c("Tails"), 
         col = c(col_tail_low), pch = 19, bty = "n", pt.cex = 2.2)
  
  
    # STILL need to think and CHECK EVERYTHING AGAIN
  
  

  # Interval_Low tail
  polygon(c(dens$x[dens$x <= interval_low], interval_low, interval_low),
          c(dens$y[dens$x <= interval_low], y_interval_low, 0),
          col = col_tail_low,
          border = NA,
          main = "")

  # High tail
  polygon(c(interval_high, dens$x[dens$x >= interval_high], interval_high),
          c(y_interval_high, dens$y[dens$x >= interval_high], 0),
          col = col_tail_high,
          border = NA,
          main = "")

  

  # To make it so the line is not partly overshadowed 
  lines(dens,
       xlab = x_lab,
       lwd = 1.2,
       xlim = x_lim,
       main = main_title,
       las=1)
  
  
  
  
  #rug(dens$x, side=3)
  rug(dat_mcmc, ticksize = 0.025, lwd=0.2)
  
  

  # Prob remove this once fixed, just easier to debug with values in
#  return(list(dens = dens,
#              low = low,
#               high = high,
#               i_low = i_low,
#               y_low = y_low,
#               i_high = i_high,
#               y_high = y_high))
}
