##' Make `create_intervals()` use the right method depending on class of input
##'
##' Based on `plot()`.
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
