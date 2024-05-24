##' Plot figure with recruitment over all years plus relative spawning biomass
##'  over recent years, likely to be Figure 2. Have to run .Rmd first. TODO tidy
##'  this up as has non-argument values in it.
##'  <desc>
##'
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
figure_recruit_biomass <- function(file = "figure-recruit_biomass.pdf",
                                   height = 4,
                                   width = 114/25.4){

 pdf(file = file,
            height = height,
            width = width,
            paper="special")

  par(mai = c(0.32, 0.3, 0.15, 0.12),
      mfrow = c(2,1),
      mgp = c(1.6,0.5,0),
      cex = 0.6)

  plot_series(recruitment_intervals)

  mtext("A", side = 3, adj = 0, cex = 0.7,
        line = 0.3) # TODO Add to function, cex is indpt of par(cex)

  plot_series(relative_biomass_res$intervals_all_years,
              y_max = 2.6,
              xlim = c(2010, 2027),
              add_line_at_0.4 = TRUE,
              inc = 0.05,
              leg_loc = "topleft",
              ylab = "Relative spawning biomass")
  mtext("B", side = 3, adj = 0, cex = 0.7,
        line = 0.3) # TODO Add to function

  dev.off()
}
