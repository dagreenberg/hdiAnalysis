##' Print a Markdown summary table of main results from `create_intervals()`
##' output of a tibble (e.g. for multiple years)
##'
##' @param int_dens_list `intervals_density_list` object as output from
##'   `create_intervals()` being applied to a tibble
##' @param dig number of decimal places to show
##' @return Markdown code (for automatic use straight in an R Markdown document, for instance)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' res <- create_intervals(rec_2021) TODO
##' summary_table(res)
##' }
summary_table.intervals_density_list <- function(int_dens_list,
                                                 dig = 2){
  stopifnot("intervals_density_list" %in% class(int_dens_list))

  int_all <- int_dens_list$intervals_all

  dplyr::mutate(int_all,
                "ETI lower" = eti_lower,
                "ETI upper" = eti_upper,
                "HDI lower" = hdi_lower,
                "HDI upper" = hdi_upper,
                "Interval difference" = width_diff,
                "Range a lower" = a_lower,          # TODO think if left-skewed
                "Range a upper" = eti_lower,
                "Range b lower" = b_lower,
                "Range b upper" = eti_upper) %>%
    dplyr::select(quantity,
                  "ETI lower":"Range b upper") %>%
    knitr::kable(digits = 2)
}
