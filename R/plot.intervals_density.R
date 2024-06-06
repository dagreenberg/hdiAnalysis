##' Plot kernel density function of an `intervals_density` object, with
##' tails shaded as specified and optional explanatory lines added
##'
##' TODO creates plot on object of class `intervals_density`, which is the
##' result of running `create_intervals()` on a vector of values.
##'
##' @param dat_mcmc a numeric vector representing an MCMC sample.
##' @param dens_intervals
##' @param year Which year of recruitment (age-0) to plot
##' @param type type of intervals: either `hdi` or `equal`
##' @param x_lim
##' @param col_main
##' @param col_tail
##' @param main_title
##' @param main_title_include logical whether to include a main title
##' @param x_lab
##' @param rug_top logical whether to show rug at the top for the density values
##' @param rug_bottom logical whether to show rug at the bottom for the raw data values
##' @param interval_arrows logical whether to show arrows depicting intervals
##' @param y_arrow value on y-axis to show the arrows depicting intervals
##' @param col_bars colour of the bars showing regions A and B
##' @param bars_multiplier numeric multiplier, so nudge the bars higher (value of
##'   1.0 puts them exactly at the height of the right-hand end of low credible interval)
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
plot.intervals_density <- function(ints_dens,
                                   dat = NULL,   # include if want rugs added
                                   type = "hdi",
                                   col_main = "blue",
                                   col_main_text = NULL,
                                   col_tail = "red",
                                   main_title = NULL,
                                   main_title_include = FALSE,
                                   hdi_horizontal = TRUE,
                                   col_hdi_horizontal = "darkgrey",
                                   rug_top = FALSE,
                                   rug_bottom = FALSE,
                                   interval_arrows = FALSE,
                                   y_arrow = 0.098,
                                   arrowhead_length = 0.2,
                                   arrowhead_gap = 0,  # half the gap between arrow heads
                                        # manually tweak to give a slight gap
                                        # between them as they default is they
                                        # touch too much. Half since applied to
                                        # each arrow
                                   col_bars = "black",
                                   bars_multiplier = 1.5,
                                   lwd_border = 0.4,
                                   x_small_ticks_add = TRUE,  # whether to add
                                        # them or not
                                   x_small_ticks_by = NULL, # use to fine tune
                                   ticks_tcl = -0.2,
                                   ...){

  if(!(type %in% c("eti", "hdi"))){
    stop("type needs to eti or hdi.")}

  # Default is to make text the same colour for main interval
  if(is.null(col_main_text)){
    col_main_text <- col_main
  }

  ints <- ints_dens$intervals
  dens <- ints_dens$density

  # Reorder Just use for title, maybe also low and high, actually prob not
  if(type == "eti"){
      if(is.null(main_title)) {
        main_title <- "Equal-tailed interval"
    }
  } else { # type == "hdi"
    if(is.null(main_title)) {
      main_title <- "Highest density interval"
    }
  }

  # TODO do the attribute axis name, may be getting too clever


  # TODO change to x_interval_low etc.
  # low and high values of the interval for plotting, already calculated
  if(type == "eti"){
     interval_low <- ints$eti_lower
     interval_high <- ints$eti_upper
     y_interval_low <- ints$y_eti_lower
     y_interval_high <- ints$y_eti_upper
  } else { # type == "hdi"
     interval_low <- ints$hdi_lower
     interval_high <- ints$hdi_upper
     y_interval_low <- ints$y_hdi_lower
     y_interval_high <- ints$y_hdi_upper
  }

  # If low end of interval is 0 then its corresponding y value is >0, and so
  # need to add a 0,0 point to make the polygons work (since they draw
  # a line back to the first point, which worked fine for low end !=
  # 0). Actually, may as well always include (0,0), which creates a hard
  # vertical line at 0 if HDI y value is >0.

  # TODO think about more
#  dens$x <- c(0, dens$x)
#  dens$y <- c(0, dens$y)

  # Get the right size
  plot(dens,
       main = "",
       col = NULL,
       zero.line = FALSE,
       # las = 1,
       ...)

  add_minor_tickmarks()

  # TODO STILL need to think and CHECK EVERYTHING AGAIN

  # Full distribution
  polygon(dens,
          col = col_main,
          border = col_main,
          lwd = lwd_border,
          main = "")

  # Interval_Low tail
  polygon(c(dens$x[dens$x <= interval_low], interval_low, interval_low),
          c(dens$y[dens$x <= interval_low], y_interval_low, 0),
          col = col_tail,
          border = col_tail,
          lwd = lwd_border,
          main = "")

  # High tail
  polygon(c(interval_high, dens$x[dens$x >= interval_high], interval_high),
          c(y_interval_high, dens$y[dens$x >= interval_high], 0),
          col = col_tail,
          border = col_tail,
          lwd = lwd_border,
          main = "")

  lines(dens,
        lwd = 0.4)

  # Add horizontal line for HDI
  if(type == "hdi" & hdi_horizontal){
    abline(h = y_interval_low,
           col = col_hdi_horizontal,
           lwd = 1)
  }

  if(rug_top){
    rug(dens$x,
        side=3)
  }

  # Data not included in function, but user could just add manually
  # if(rug_bottom){
  #  rug(dat_mcmc)
  #}

  if(interval_arrows){
    # 95% interval
    shape::Arrows(interval_low + arrowhead_gap,
                  y_arrow,
                  interval_high - arrowhead_gap,
                  y_arrow,
                  code = 3,
                  col = col_main,
                  arr.type = "triangle",
                  arr.adj = 1,
                  arr.length = arrowhead_length)
    text(mean(c(interval_low, interval_high)),
         y_arrow,
         "95%",
         col = col_main_text,
         pos = 3)
    # Left-hand tail
    shape::Arrows(0,
                  y_arrow,
                  interval_low - arrowhead_gap,
                  y_arrow,
                  code = 3,
                  col = col_tail,
                  arr.type="triangle",
                  arr.adj = 1,
                  arr.length = arrowhead_length)
    # Right-hand tail
    shape::Arrows(interval_high + arrowhead_gap,
                  y_arrow,
                  par("usr")[2],
                  y_arrow,
                  code = 1,
                  col = col_tail,
                  arr.type="triangle",
                  arr.adj = 1,
                  arr.length = arrowhead_length)
    # Annotate if ETI
    if(type == "eti"){
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
  if(type == "eti"){
    # Left-hand bar: area of excluded values but more probable than parts of upper
    # tail. Right side of bar:
    i_right_side <- max(which(dens$y < y_interval_high & dens$x <= interval_high))

    shape::Arrows(
             #      dens$x[dens$x > dens$x[i_right_side] & dens$x <= interval_low], interval_low, interval_low, dens$x[i_right_side])
             dens$x[i_right_side],
             y_interval_low * bars_multiplier,
             interval_low,
             y_interval_low * bars_multiplier,
             lwd = 2,
             code = 3,
             col = col_bars,
             arr.type = "T",
             arr.adj = 1,
             arr.length = arrowhead_length/2)

    text(mean(c(dens$x[i_right_side],
                interval_low)),
         y_interval_low * bars_multiplier,
         "a",
         col = col_bars,
         pos = 3)

    ## polygon(c(dens$x[i_right_side], dens$x[dens$x > dens$x[i_right_side] & dens$x <= interval_low], interval_low, interval_low, dens$x[i_right_side]),
    ##          c(dens$y[i_right_side], dens$y[dens$x > dens$x[i_right_side] & dens$x <= interval_low], y_interval_low, 0, 0),
    ##          col = col_excluded,
    ##          border = NA,
    ##          main = "")

    # Right-hand bar: area of included values but as probable as some of lower
    # tail. Left side of bar:
    i_left_side <- max(which(dens$y > y_interval_low))


    shape::Arrows(dens$x[i_left_side + 1],
                  y_interval_low * bars_multiplier,
                  interval_high,
                  #max(dens$x[dens$x > dens$x[i_left_side] & dens$x <=
                  #           interval_high]),  # TODO is that not just interval_high?
                  y_interval_low * bars_multiplier,
                  lwd = 2,
                  code = 3,
                  col = col_bars,
                  arr.type = "T",
                  arr.adj = 1,
                  arr.length = arrowhead_length/2)

    text(mean(c(dens$x[i_left_side + 1],
                interval_high)),
         y_interval_low * bars_multiplier,
         "b",
         col = col_bars,
         pos = 3)

   # need to generalise
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
  }
}
