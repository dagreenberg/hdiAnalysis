#' With Warnings
#'
#' This function is used by chi.2.eksp to ensure that the function works correctly. It makes use
#' of functions that provide a mechanism for handling unusual conditions, including errors and warnings.
#' A list is returned of the value and the associated warning
#'
#' Taken from https://rdrr.io/github/Boshoffsmit/novaReport/src/R/withWarnings.R
#'   and then adapted. TODO update help
#' @param expr expression to be evaluated
#' @references Bill Dunlap - original co-author
#' @export


with_warnings <- function (expr)
{
  warnings <- character()
  retval <- withCallingHandlers(expr, warning = function(ex) {
    warnings <<- c(warnings, conditionMessage(ex))
    invokeRestart("muffleWarning")
  })
  list(value = retval,
       warnings = warnings)
}
