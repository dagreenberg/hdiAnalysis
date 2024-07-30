##' Make `summary_table()` use the right method depending on class of input
##'
##' Based on `plot()`.
##'
##' @param dat
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
summary_table <- function(dat, ...){
  UseMethod("summary_table")
}
