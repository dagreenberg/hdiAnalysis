##' Calculate an intermediate y value between two (x,y) points.
##'
##' Given x1 and x2 and corresponding y1 and y2, calculate the intermediate
##'  value at `x_val` where `x1 <= x_val <= x2`, by a simple linear interpolation.
##'
##' @param x_val
##' @param x1
##' @param x2
##' @param y1
##' @param y2
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' intermediate_y(5, 4, 6, 10, 17)
##' }
intermediate_y <- function(x_val,
                           x1,
                           x2,
                           y1,
                           y2){
  stopifnot(x1 < x2 & x1 <= x_val & x_val <= x2)
  # y1 + gradient            * how far along
  y1 + (y2 - y1) / (x2 - x1) * (x_val - x1)
}
