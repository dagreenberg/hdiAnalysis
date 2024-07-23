##' Plot a time series of intervals from multiple samples calculated from
##' `create_intervals()` applied to a data frame object.
##'
##' TODO can maybe delete this I think. Expect it got used for default plotting
##' functions. Is is hdi-analysis.Rmd, so think about if that needs rerunning
##' again; may just want to keep the pdf for historical reasons, as other
##' vignettes now show new calculations..
##'
##' Will show ETIs, HDIs, or both.
##'
##' TODO adapting from `plot_series` which I can delete once finalised.
##'
##' Adapted from `pacea::plot.pacea_recruitment()`.
##'
##' TODO Temporal plot for a time series (of class
##' `pacea_recruitment`) object. The `style` option here (unlike for
##' `plot.pacea_index()` defaults to `no_uncertainty` and gets changed to
##' `uncertainty` if `low` and `high` are columns of `obj`.
##'
##' @param obj a `pacea_recruitment` object, which is a time series. Function
##'   will run on other objects (not give an error) but is not tested on those.
##' @param value the column to plot if no uncertainties, or what to plot as dots
##'   if showing uncertainties (likely always `median`)
##' @param style `no_uncertainty` for plain time series without uncertainty,
##'   gets overridden to have uncertainty bars if `low` and `high` are columns
##'   of `obj`
##' @param uncertainty_bar_col colour for uncertainty bars for certain types of
##'   plot (e.g. estimated fish recruitment)
##' @param y_max maximum y value for certain types of plot (use this if you get
##'   an error when specifying `ylim`)
##' @param add_line_at_1 whether to add a horizontal line at 1 (only sensible for scaled recruitments)
##' @param add_line_at_1_col colour for line at 1
##' @param add_line_at_1_lty line type of line at 1
##' @param ... further options passed onto `plot.default()`
##' @return plot of the time series as median with bars showing uncertainty (if
##'   `low` and `high` are columns of `obj) to the current device; returns nothing.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' recruitment_res <- calc_density_many_years(dplyr::select(hake_recruitment_mcmc,
##'                                                          -"Virgin"))     # TODO
##' plot_series(recruitment_res$dens_intervals)
##' }
plot_series <- function(obj,   # the $intervals_all_years output from calc_density_many_years()
                        xlab = "Year",
                        ylab = "Recruitment (billions of fish)",
                        inc = 0.15,
                        x_tick_extra_years = 20,
                        start_decade_ticks = (min(obj$year) %/% 10) * 10,
                        eti_bar_col = "blue",
                        hdi_bar_col = "red",
                        y_max = 35,          # default for recruitment
                        add_line_at_0.4 = FALSE,
                        add_line_at_0.4_col = "darkgreen",
                        add_line_at_0.4_lty = 5,
                        y_tick_start = 0,
                        y_tick_end = NULL,
                        y_tick_by = 1,
                        cex_val = 0.8,   # Size of points for medians
                        inset = c(0, 0), #c(0.1,-0.02),   # For shifting legend
                        add_legend = TRUE,
                        leg_loc = "topright",
                        join_intervals = FALSE, # join up the ends of the
                                        # intervals, useful for sample size plot
                        arrowhead_length = 0.15,
                        ...
                                   ){
# pacea was this:
#  year    low median  high
#   <dbl>  <dbl>  <dbl> <dbl>
# 1  1966 0.0642  1.54   9.84
# 2  1967 0.239   4.52  14.1


# Here have this:
# recruitment_res$intervals_all_years
# A tibble: 62 × 17
#    year median  `2.5` `97.5` hdi_lower hdi_upper width_equal width_hdi
#   <dbl>  <dbl>  <dbl>  <dbl>     <dbl>     <dbl>       <dbl>     <dbl>
# 1  1966  1.63  0.0571  10.7    -0.671       8.60       10.6       9.27
# 2  1967  4.65  0.214   14.9    -0.580      12.7        14.7      13.3

  if(is.null(y_max)){
    y_max = max(c(obj$`97.5`,
                  obj$hdi_upper))
  }

  # Adapting from pacea::plot_with_uncertainty_discrete(), not using dates as
  # only worrying about years here.

  # ETI:
  eti_year <- obj$year - inc        # Shift ETI ones to the left
  plot(eti_year,
       obj[["median"]], # [[]] returns a vector not a tibble
       xlab = xlab,
       ylab = ylab,
       pch = 20,
       ylim = c(0, y_max),   # specifying ylim in main plot call won't override this
       cex = cex_val,
       ...)
  # abline(h = 0, col = "lightgrey")

  segments(x0 = eti_year,
           y0 = obj$`2.5`,
           x1 = eti_year,
           y1 = obj$`97.5`,
           col = eti_bar_col)

  points(eti_year,
         obj[["median"]], # [[]] returns a vector not a tibble
         pch = 20,        # plot points again to be on top of bars
         cex = cex_val)

  # HDI:
  hdi_year <- obj$year + inc        # Shift HDI ones to the right

  # HDI:
  segments(x0 = hdi_year,
           y0 = obj$hdi_lower,
           x1 = hdi_year,
           y1 = obj$hdi_upper,
           col = hdi_bar_col)

  points(hdi_year,
         obj[["median"]], # [[]] returns a vector not a tibble
         pch = 20,
         cex = cex_val)         # plot points again to be on top of bars

  # abline(h = 0, col = "lightgrey")

  if(join_intervals){
    lines(eti_year,
          obj$`2.5`,
          col = eti_bar_col,
          lty = 2)

    lines(eti_year,
          obj$`97.5`,
          col = eti_bar_col,
          lty = 2)

    lines(hdi_year,
          obj$hdi_lower,
          col = hdi_bar_col,
          lty = 2)

    lines(hdi_year,
          obj$hdi_upper,
          col = hdi_bar_col,
          lty = 2)
  }

  # For relative biomass plots
  if(add_line_at_0.4){
    abline(h = 0.4,
           col = add_line_at_0.4_col,
           lty = add_line_at_0.4_lty)
    # Also denote 'now' and 'projections', likely only want if adding the line
    # (implying relative spawning biomass plots; TODO should generalise).
    text(2024,
         0.07,
         "Now")
         # adj = c(0.5, 1))
#         pos = 4)
    shape::Arrows(2025,
                  -0.05,
                  2027.5,
                  -0.05,
                  lwd = 1,
                  code = 2,
                  col = "black",,
                  arr.type = "triangle",
                  arr.adj = 1,
                  arr.length = arrowhead_length)
    text(2024.7,
         0.05,
         "Projections",
         pos = 4)
  }


  # Adapted from pacea::add_tickmarks():

  min = min(obj$year)
  max = max(obj$year)
  axis(1,
       seq(min,
           max),
       labels = FALSE,
       tcl = -0.2)

  # Slightly larger ticks every decade (since not all get labelled automatically)
  axis(1,
       seq(start_decade_ticks,
           max,
           by = 10),
       labels = FALSE,
       tcl = -0.3)

  # y-axis tickmarks:
  if(is.null(y_tick_start)){
    y_tick_start <- floor(par("usr")[3])
  }
  if(is.null(y_tick_end)){
    y_tick_end  <- ceiling(par("usr")[4])
  }

  axis(2,
       seq(y_tick_start,
           y_tick_end,
           by = y_tick_by),
       labels = FALSE,
       tcl = -0.2)

  if(add_legend){
    legend(leg_loc,
           legend = c("Equal-tailed interval",
                      "Highest density interval"),
           lty = 1,
           lwd = 2,
           col = c(eti_bar_col,
                   hdi_bar_col),
           bty = "n",
           inset = inset)
  }


#  add_tickmarks(obj,
#                y_tick_by = y_tick_by,
#                y_tick_start = 0,
#                y_tick_end = ceiling(par("usr")[4]),
#                x_tick_extra_years = x_tick_extra_years,
#                start_decade_ticks = start_decade_ticks)

  invisible()
}
