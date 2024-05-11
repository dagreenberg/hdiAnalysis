##' Create credible intervals from a numeric vector
##'
##'  <desc>
##'
##' @param dat a numeric vector consisting of MCMC samples that we want to
##'   summarise with credible intervals
##' @return tibble of one row (to be consistent with
##'   `create_intervals.data.frame()`) with columns `median`, `2.5` and `97.5`
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
##' rec_intervals <- create_intervals(dplyr::pull(hake_recruitment_mcmc, "2021")
##' }
create_intervals.numeric <- function(dat){
  hdi_res <- HDInterval::hdi(dat)
  res <- tibble("median" = median(dat),
                "2.5" = quantile(dat, probs = c(0.025)),
                "97.5" = quantile(dat, probs = c(0.975)),
                "hdi_lower" = hdi_res["lower"],
                "hdi_upper" = hdi_res["upper"]) %>%

    mutate(width_equal = `97.5` - `2.5`,
           width_hdi = hdi_upper - hdi_lower,
           width_diff = width_equal - width_hdi)

  res
}
