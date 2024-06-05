##' Add minor unlabelled tickmarks to an existing plot
##'
##' Add sensible smaller (unlabelled) tickmarks to both axes of an existing
##' plot. Adapted from `pacea::add_tickmarks()`. Is exported but unlikely to be needed externally.
##'
##' @param x_tick_start
##' @param x_tick_by
##' @param x_tick_end
##' @param y_tick_start
##' @param y_tick_by
##' @param y_tick_end
##' @return adds minor tickmarks to an existing plot
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
add_minor_tickmarks <- function(x_tick_start = 0,
                                x_tick_by = 2,
                                x_tick_end = NULL,
                                y_tick_start = 0,
                                y_tick_by = 0.01,
                                y_tick_end = NULL){
  if(is.null(x_tick_end)){
    x_tick_end  <- ceiling(par("usr")[2])
  }

  if(is.null(y_tick_end)){
    y_tick_end  <- ceiling(par("usr")[4])
  }

  # Small ticks every x_tick_by
  axis(1,
       seq(x_tick_start,
           x_tick_end,
           by = x_tick_by),
       labels = FALSE,
       tcl = -0.2)

  # Slightly larger ticks every decade (since not all get labelled automatically)
  ## axis(1,
  ##      seq(start_decade_ticks,
  ##          max,
  ##          by = "10 years"),
  ##      labels = FALSE,
  ##      tcl = -0.3)

  axis(2,
       seq(y_tick_start,
           y_tick_end,
           by = y_tick_by),
       labels = FALSE,
       tcl = -0.2)
}
