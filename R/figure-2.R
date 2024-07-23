##' Fig 2
##'  <desc>
##'
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @ontrun{
##' @
##' @}
figure_2 <- function(file = "figure-2.pdf",
                     height = 4,
                     width = 114/25.4){
  # From figure_recruit_biomass which currently made figure 2
  pdf(file = file,
      height = height,
      width = width,
      paper="special")

  par(mai = c(0.32, 0.3, 0.15, 0.12),
      mfrow = c(2,1),
      mgp = c(1.6,0.5,0),
      cex = 0.6)

  res_all_years <- create_intervals(dplyr::select(hake_recruitment_mcmc,
                                                  -"Virgin"))

  plot(res_all_years,
       xlab = "Year",
       ylab = "Recruitment (billions of fish)")

  mtext("A",
        side = 3,
        adj = 0,
        cex = 0.7,
        line = 0.3) # TODO Add to function, cex is indpt of par(cex)

##   plot_series(relative_biomass_res$intervals_all_years,
##               y_max = 2.6,
##               xlim = c(2010, 2027),
##               add_line_at_0.4 = TRUE,
##               inc = 0.05,
##               leg_loc = "topleft",
##               ylab = "Relative spawning biomass")
##   mtext("B", side = 3, adj = 0, cex = 0.7,
##         line = 0.3) # TODO Add to function

   dev.off()
## }
  # Will need these to tailor these figures, but also see
  # Taking these out of the more generic
  # plot.intervals_density_list:
  ## inc = 0.15,
  ## x_tick_extra_years = 20,
  ## start_decade_ticks = (min(obj$year) %/% 10) * 10,
  ## eti_bar_col = "blue",
  ## hdi_bar_col = "red",
  ## y_max = 35,          # default for recruitment may need just ylim = c(0, 35)
  ## add_line_at_0.4 = FALSE,
  ## add_line_at_0.4_col = "darkgreen",
  ## add_line_at_0.4_lty = 5,
  ## y_tick_start = 0,
  ## y_tick_end = NULL,
  ## y_tick_by = 1,
  ## cex_val = 0.8,   # Size of points for medians
  ## inset = c(0, 0), #c(0.1,-0.02),   # For shifting legend
  ## add_legend = TRUE,
  ## leg_loc = "topright",
  ## join_intervals = FALSE, # join up the ends of the
  ## # intervals, useful for sample size plot
  ## arrowhead_length = 0.15,
}
