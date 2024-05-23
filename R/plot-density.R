##' Plot density function (smoothed) of MCMC recruitment for a given year, with tails shaded as specified
##'
##' @param dat_mcmc a numeric vector representing an MCMC sample.
##' @param dens_intervals
##' @param year Which year of recruitment (age-0) to plot
##' @param type type of intervals: either `hdi` or `equal`
##' @param x_lim
##' @param col_main
##' @param col_tail
##' @param main_title
##' @param x_lab
##' @param rug_top logical whether to show rug at the top for the density values
##' @param rug_bottom logical whether to show rug at the bottom for the raw data values
##' @param interval_arrows logical whether to show arrows depicting intervals
##' @param y_arrow value on y-axis to show the arrows depicting intervals
##' @param ... arguments to pass onto `plot()`
##' @param rec_intervals result of `create_intervals(dat_mcmc)`; is calculated
##'   if not supplied. May be worth supplying so it's not being repeatedly calculated.
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
                         col_main = "yellow3",  # worse than blue!
                         col_tail = "red",
                         main_title = NULL,
                         x_lab = NULL,
                         rug_top = FALSE,
                         rug_bottom = FALSE,
                         interval_arrows = FALSE,
                         y_arrow = 0.095,

                         ...){
  if(!(type %in% c("equal", "hdi"))){
    stop("type needs to equal or hdi.")}

  if(is.null(dens_intervals)){
    dens_intervals <- calc_density(dat_mcmc)
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

  # If low end of interval is 0 then its corresponding y value is >0, and so
  # need to add a 0,0 point to make the polygons work (since they draw
  # a line back to the first point, which worked fine for low end !=
  # 0). Actually, may as well always include (0,0), which creates a hard
  # vertical line at 0 if HDI y value is >0.

  dens$x <- c(0, dens$x)
  dens$y <- c(0, dens$y)

  plot(dens,
       xlab = x_lab,
       lwd = 2,
       xlim = x_lim,
       main = main_title,
       ...)
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
  # abline(h = y_interval_low)

  if(rug_top){
    rug(dens$x,
        side=3)
  }

  if(rug_bottom){
    rug(dat_mcmc)
  }

  if(interval_arrows){
    # 95% interval
    arrows(interval_low,
           y_arrow,
           interval_high,
           y_arrow,
           code = 3,
           col = col_main)
    text(mean(c(interval_low, interval_high)),
         y_arrow,
         "95%",
         col = col_main,
         pos = 3
         )
    # Left-hand tail
    arrows(0,
           y_arrow,
           interval_low,
           y_arrow,
           code = 2,
           col = col_tail)
    # Right-hand tail
    arrows(interval_high,
           y_arrow,
           par("usr")[2],
           y_arrow,
           code = 1,
           col = col_tail)
    # Annotate if ETI
    if(type == "equal"){
      text(interval_low/2,
           y_arrow,
           "2.5%",
           col = col_tail,
           pos = 3)
      text(mean(c(interval_high, par("usr")[2])),
           y_arrow,
           "2.5%",
           col = col_tail,
           pos = 3)
    }

  }

  # Show included/exluded values for ETI
  if(type == "equal"){
    # Right-hand bar: area of included values but as probable as some of lower
    # tail. Left side of bar:
    i_left_side <- max(which(dens$y > y_interval_low))


    lines(c(dens$x[i_left_side + 1],
            max(dens$x[dens$x > dens$x[i_left_side] & dens$x <=
            interval_high])),  # TODO is that not just interval_high?
      c(y_interval_low, y_interval_low), # need to generalise
          lwd = 2,
          col = "darkgreen") # need to generalise
    # TODO Marie had +1 in first bit below, think about; might be because of
    # polygon not line - now added in above, it's to stop it crossing over main curve

    ## polygon(c(dens$x[i_left_side+1], dens$x[dens$x > dens$x[i_left_side] &
    ##       dens$x <= interval_high], interval_high, interval_high,
    ##       dens$x[i_left_side+1]),
    ##       c(dens$y[i_left_side +1], dens$y[dens$x > dens$x[i_left_side] & dens$x
    ##       <= interval_high], y_interval_high, 0, 0),
    ##       col = col_included,
    ##       border = NA,
    ##       main = "")

  # Left side: area of excluded values but more probable than parts of upper
  # tail. Right side of bar:
  i_right_side <- max(which(dens$y < y_interval_high & dens$x <= interval_high))

    lines(c(
#      dens$x[dens$x > dens$x[i_right_side] & dens$x <= interval_low], interval_low, interval_low, dens$x[i_right_side])
      dens$x[i_right_side],
      interval_low),
      c(y_interval_low, y_interval_low), # TODO genralise and col
      lwd = 2,
      col = "darkgreen")

  ## polygon(c(dens$x[i_right_side], dens$x[dens$x > dens$x[i_right_side] & dens$x <= interval_low], interval_low, interval_low, dens$x[i_right_side]),
  ##          c(dens$y[i_right_side], dens$y[dens$x > dens$x[i_right_side] & dens$x <= interval_low], y_interval_low, 0, 0),
  ##          col = col_excluded,
  ##          border = NA,
  ##          main = "")
  }

}
