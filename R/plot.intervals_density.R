##' Plot kernel density function of an `intervals_density` object, with
##' tails shaded as specified and optional explanatory lines added
##'
##' TODO creates plot on object of class `intervals_density`, which is the
##' result of running `create_intervals()` on a vector of values.
##'
##' @param ints_dens
##' @param dat
##' @param type type of intervals: either `hdi` or `equal`
##' @param col_main
##' @param col_main_text
##' @param col_tail
##' @param main_title
##' @param col_hdi_horizontal
##' @param rug_top logical whether to show rug at the top for the density values
##' @param rug_bottom logical whether to show rug at the bottom for the raw data values
##' @param interval_arrows logical whether to show arrows depicting intervals
##' @param y_arrow value on y-axis to show the arrows depicting intervals
##' @param arrowhead_length
##' @param arrowhead_gap
##' @param col_bars colour of the bars showing regions A and B
##' @param bars_multiplier numeric multiplier, to nudge the bars higher (value of
##'   1.0 puts them exactly at the minimum density of the ends of the credible
##'   interval)
##' @param lwd_border
##' @param show_a_b logical, whether to show intervals a and b on ETI plot
##' @param explanatory_lines_a_b logical, whether to plot extra lines to explain the
##'   ranges a and b for the ETI.
##' @param explanatory_lines_extra logical, whether to plot extra lines to explain the
##'   ranges c and d for which all values in c are more likely than those in d,
##'   yet c is outside the ETI and d is inside it.
##' @param ... arguments to pass onto `plot()`
##' @param dat_mcmc a numeric vector representing an MCMC sample.
##' @param main_title_include logical whether to include a main title
##' @param show_discontinuity logical, if `TRUE` then plot the discontinuity in
##'   the HDI that arises (only matters if `ints_dens$intervals$warning ==
##'   TRUE`).
##' @param discontinuity_multiplier numeric, y-axis value to plot points showing
##'   discontinuities, multiplies the minimum density of the ends of the
##'   credible interval.
##' @param rec_intervals result of `create_intervals(dat_mcmc)`; is calculated
##'   if not supplied. May be worth supplying so it's not being repeatedly calculated.
##' @return invisible
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' res <- create_intervals(rec_2021)
##' plot(res)
##' # And see results and result-extra vignettes.
##' }
plot.intervals_density <- function(ints_dens,
                                   dat = NULL,   # include if want rugs added
                                   type = "hdi",
                                   col_main = "blue",
                                   col_main_text = NULL,
                                   col_tail = "red",
                                   main_title = NULL, # might not do anything
                                        # now, need no main for dens plot
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
                                   # x_small_ticks_add = TRUE,  # whether to add
                                        # them or not
                                   x_minor_ticks_by = 1, # increment for adding
                                   # minor tick marks
                                   y_minor_ticks_by = 0.01,
                                   ticks_tcl = -0.2,
                                   show_a_b = TRUE,
                                   explanatory_lines_a_b = FALSE,
                                   explanatory_lines_extra = FALSE,
                                   show_discontinuity = FALSE,
                                   discontinuity_multiplier = 2,
                                   ...){

  if(!(type %in% c("eti", "hdi"))){
    stop("type needs to be eti or hdi.")}

  # Default is to make text the same colour for main interval
  if(is.null(col_main_text)){
    col_main_text <- col_main
  }

  ints <- ints_dens$intervals
  dens <- ints_dens$density
  credibility <- ints_dens$credibility
  eti_lower_percentile <- (1 - credibility)/2 * 100   # For annotating

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

  # Check here whether to draw the left-hand tail in red - don't want to if
  # hdi_lower is the min of the data (before concatenating the zero).
  # Did have this also but don't think needed. Can't actually not be ETI, but
  # have this for clarity anyway.
  if(ints$hdi_lower == min(dens$x & type == "eti")){
    fill_left_tail = FALSE} else {
                            fill_left_tail = TRUE}


  # TODO think about more, putting back in again now have one solution for zero
  # issue, to see if plot works again.
  # HERE not sure about this, think clearly
#  if(ints$allow_hdi_zero & ints$hdi_lower == 0){
    dens$x <- c(0, dens$x)
    dens$y <- c(0, dens$y)
#  }

  # Get the right size
  plot(dens,
       main = "",
       col = NULL,
       zero.line = FALSE,
       # las = 1,
       ...)

  add_minor_tickmarks(x_tick_by = x_minor_ticks_by,
                      y_tick_by = y_minor_ticks_by)   # TODO make more general, esp for y-axis

  # TODO STILL need to think and CHECK EVERYTHING AGAIN

  # Full distribution
  polygon(dens,
          col = col_main,
          border = col_main,
          lwd = lwd_border,
          main = "")

  # Interval_Low tail, but don't do one if not needed
  if(fill_left_tail){
    polygon(c(dens$x[dens$x <= interval_low], interval_low, interval_low),
            c(dens$y[dens$x <= interval_low], y_interval_low, 0),
            col = col_tail,
            border = col_tail,
            lwd = lwd_border,
            main = "")
  }
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
    abline(h = min(c(y_interval_low,
                     y_interval_high)),  # If not a true HDI then use the min
                                         # TODO think more, and test with left-skewed
           col = col_hdi_horizontal,
           lwd = 1.5)
    box()
  }

  # Add explanatory lines for ETI
  if(type == "eti" & explanatory_lines_a_b){
    abline(h = c(y_interval_low,
                 y_interval_high),
           col = col_hdi_horizontal,
           lwd = 1)
    abline(v = c(interval_low,
                 interval_high,
                 ints$a_lower,
                 ints$b_lower),
           col = col_hdi_horizontal,  # TODO change name
           lwd = 1)                   # TODO generalise
  }

  if(type == "eti" & explanatory_lines_extra){
    # Add example line (at the mean) that then defines the interval on left that is
    # outside ETI but more likely than the interval on right inside ETI - see
    # results-extra vignette.
    abline(h = c(y_interval_low,
                 y_interval_high),
           col = col_hdi_horizontal,
           lwd = 1)

    green_density <- mean(c(y_interval_low,
                            y_interval_high)) # density for green line

    i_green_left_min <- min(which(dens$y > green_density))

    i_green_right_min <- max(which(dens$y > green_density))


    abline(h = green_density,
           col = "darkgreen",
           lwd = 2)                   # TODO generalise

    abline(v = c(interval_low,
                 interval_high,
                 dens$x[i_green_left_min],
                 dens$x[i_green_right_min]),
           col = col_hdi_horizontal,  # TODO change name
           lwd = 1)                   # TODO generalise

    # Also draw the new intervals:
    shape::Arrows(dens$x[i_green_left_min],
                  y_interval_low * bars_multiplier,
                  interval_low,
                  y_interval_low * bars_multiplier,
                  lwd = 2,
                  code = 3,
                  col = col_bars,
                  arr.type = "T",
                  arr.adj = 1,
                  arr.length = arrowhead_length/2)

    text(mean(c(dens$x[i_green_left_min],
                interval_low)),
         y_interval_low * bars_multiplier,
         "c",
         col = col_bars,
         pos = 3)


    # Right-hand bar: area of included values but as probable as some of lower
    # tail. Left side of bar:

    shape::Arrows(dens$x[i_green_right_min],
                  y_interval_low * bars_multiplier,
                  interval_high,
                  y_interval_low * bars_multiplier,
                  lwd = 2,
                  code = 3,
                  col = col_bars,
                  arr.type = "T",
                  arr.adj = 1,
                  arr.length = arrowhead_length/2)

    text(mean(c(dens$x[i_green_right_min],
                interval_high)),
         y_interval_low * bars_multiplier,
         "d",
         col = col_bars,
         pos = 3)



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
    # main interval (usually 90% or 95%)
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
         paste0(credibility * 100, "%"),
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
           paste0(eti_lower_percentile, "%"),
           col = col_tail,
           pos = 3)
      text(mean(c(interval_high, par("usr")[2])),
           y_arrow,
           paste0(eti_lower_percentile, "%"),
           col = col_tail,
           pos = 3)
    }
  }

  # If HDI is discontinuous then show the regions above the critical value of
  # y (defined as W in the Appendix) that are outside the HDI; expect they
  # are in the tail and only small. Just plot circles to visualise
  if(type == "hdi" & show_discontinuity & ints$warning){

    # If not a true HDI
    # then pick the min:  TODO can move this earlier as needed elsewhere also
    y_interval_critical <-  min(y_interval_low,
                                y_interval_high)

    i_x_outside_hdi <- which(dens$x < interval_low |
                             dens$x > interval_high)

    i_y_above_critical <- which(dens$y > y_interval_critical)

    i_to_plot <- intersect(i_x_outside_hdi,
                           i_y_above_critical)

    points(dens$x[i_to_plot],
           rep(y_interval_critical * discontinuity_multiplier,
               length(i_to_plot)))
  }


  # Show intervals a and b
  if(type == "eti" & show_a_b){
    # TODO does this work if left-skewed?? Don't think it does. Need to adapt
    #  (figure out when testing).
    # TODO also rewrite as a and b to match figure, though need to think about
    #  left-skewed again. See summary_table() also as writing that new and may
    #  make it simpler than this.

    shape::Arrows(
             #      dens$x[dens$x > dens$x[i_right_side] & dens$x <= interval_low], interval_low, interval_low, dens$x[i_right_side])
             ints$a_lower,
             y_interval_low * bars_multiplier,
             interval_low,
             y_interval_low * bars_multiplier,
             lwd = 2,
             code = 3,
             col = col_bars,
             arr.type = "T",
             arr.adj = 1,
             arr.length = arrowhead_length/2)

    text(mean(c(ints$a_lower,
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

    shape::Arrows(ints$b_lower,
                  y_interval_low * bars_multiplier,
                  interval_high,
                  y_interval_low * bars_multiplier,
                  lwd = 2,
                  code = 3,
                  col = col_bars,
                  arr.type = "T",
                  arr.adj = 1,
                  arr.length = arrowhead_length/2)

    text(mean(c(ints$b_lower,
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
