##' Make `create_intervals()` use the right method depending on class of input
##'
##' Based on `plot()`. The functions are `create_itervals.numeric()` and `create_intervals.data.frame()`.
##' @param dat
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
create_intervals <- function(dat, ...){
  UseMethod("create_intervals")
}
