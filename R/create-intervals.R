##' Create credible intervals from a tibble
##'  <desc>
##'
##' @param dat a tibble where each row represents an MCMC sample, and each
##'   column is a quantity which we want to summarise with credible intervals.
##' @return tibble with rows representing the columns of `dat`, and columns
##'   `name` (giving each column of `dat`), `median`, `2.5` and `97.5`
##'   representing the equal-tailed interval, `hdi_lower` and `hdi_upper`
##'   representing the lower and upper ends of the HDI 95% interval,
##'   `width_equal` and `width_hdi` giving the widths of the two types of
##'   intervals, and `width_diff` giving the difference between them
##'   (`width_equal - width_hdi`, which must be non-negative
##'   because HDI by definition is the narrowest interval).
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' rec_intervals <- create_intervals(hake_recruitment_mcmc)
##' }
create_intervals <- function(dat){
  res <- tibble("name" = names(dat),
                "median" = apply(dat, 2, median),
                "2.5" = apply(dat, 2, quantile, probs = c(0.025)),
                "97.5" = apply(dat, 2, quantile, probs = c(0.975)),
                "hdi_lower" = apply(dat, 2, HDInterval::hdi)["lower", ],
                "hdi_upper" = apply(dat, 2, HDInterval::hdi)["upper", ]) %>%
                                        # A little inefficient as calcs hdi
                                        # twice
    mutate(width_equal = `97.5` - `2.5`,
           width_hdi = hdi_upper - hdi_lower,
           width_diff = width_equal - width_hdi)

 res
}
