##' Print a Markdown summary table of main results from `create_intervals()` output
##'
##' @param int_dens `intervals_density` object as output from `create_intervals()`
##' @param dig number of decimal places to show
##' @return Markdown code (for automatic use straight in an R Markdown document, for instance)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' res <- create_intervals(rec_2021)
##' summary_table(res)
##' }
summary_table <- function(int_dens,
                          dig = 2){
  stopifnot("intervals_density" %in% class(int_dens))

  int <- int_dens$intervals
  dens <- int_dens$density

  # TODO think about left-skewed
  #  i_for_interval_a_low <- max(which(dens$y < int$eti_upper & dens$x <= int$eti_lower))
  i_for_interval_a_low <- min(which(dens$y > int$y_eti_upper &
                                    dens$x < int$eti_lower))   # TODO think about if
                                        # empty, test on symmetric
  interval_a <- c(dens$x[i_for_interval_a_low],
                  int$eti_lower)    # TODO adapt based on above thought

  i_for_interval_b_low <- min(which(dens$y < int$y_eti_lower &
                                    dens$x > int$eti_lower))   # TODO think about if
                                        # empty, test on symmetric
  interval_b <- c(dens$x[i_for_interval_b_low],
                  int$eti_upper)    # TODO adapt based on above thought


  cat("| Quantity| Value | Interval width | \n",
      "| :-------| -----:| -----:| \n",
      "| Median  |", f(res_2021$intervals$median, dig) ,"| -- | \n",
      "| ETI     |", f(res_2021$intervals$eti_lower, dig), "--",
        f(res_2021$intervals$eti_upper, dig), "|", f(res_2021$intervals$width_eti,
                                                     dig), "| \n",
      "| HDI     |", f(res_2021$intervals$hdi_lower, dig), "--",
        f(res_2021$intervals$hdi_upper, dig), "|", f(res_2021$intervals$width_hdi,
                                                     dig),  "| \n",
      "| Range a |", f(interval_a[1], dig), "--", f(interval_a[2], dig), "|",
        f(interval_a[2] - interval_a[1], dig), "| \n",
      "| Range b |", f(interval_b[1], dig), "--", f(interval_b[2], dig), "|",
        f(interval_b[2] - interval_b[1], dig), "| \n"
      )
}
