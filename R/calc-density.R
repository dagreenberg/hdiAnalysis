##' For a vector of values, calculate the equal and HDI intervals and the kernel
##'  density with corresponding equal and HDI intervals
##'
##' @param dat_mcmc
##' @param intervals
##' @param density if TRUE (the default) then use the density approach for the HDI
##'   calculation, rather than the `hdi()` default of just the sample values.
##' @param ... arguments to pass to `create_intervals_density()`, that then get
##'   passed onto `density()` TODO check as also have directly to `density()`
##'   now, seems to be okay?
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' one_year_mcmc <- dplyr::pull(hake_recruitment_mcmc, `2021`)
##' calc_density(one_year_mcmc)
##' }
calc_density <- function(dat_mcmc,
                         intervals = NULL,
                         density = TRUE,
                         ...
                         ){
  if(is.null(intervals)){
    if(density){
      intervals <- create_intervals_density(dat_mcmc,
                                            ...)  # tibble, even if only one row
    }else{
      intervals <- create_intervals.numeric(dat_mcmc)  # tibble, even if only one row
    }

  }

  dens <- density(dat_mcmc,
                  ...)       # Gives values with equal spacing, so high
                                  # resolution in the tail which has sparse data


  # old thoughts, might need some:
  # Calculate the pdf value at low and high x values, have to interpolate.
  # Don't need to for HDI, since values will be exact x values, which I hadn't
  # realised when first doing this. (Need to check about 2.5 and 97.5).
  # And since intervals are considered [ , ) and HDI intervals are actually
  # bigger than the 'true' HDI interval, my original interpolation was actually,
  # I think, just selecting the i_high+1 value by mistake. See .Rmd for
  # thoughts.
  # Actually no, dens is a smoothed version. Think I just need to go the other
  # way for the high value??

  # Need a value of y corresponding to the low and high ends of equal and HDI
  # intervals, so have to interpolate

  # Equal intervals
  i_low_equal <- findInterval(intervals$`2.5`,
                              dens$x)  # low is between dens$x[i_low] and dens$x[i_low + 1]
  y_low_equal_interp <- dens$y[i_low_equal] +
    (dens$y[i_low_equal + 1] - dens$y[i_low_equal])/(dens$x[i_low_equal + 1] - dens$x[i_low_equal]) *
    (intervals$`2.5` - dens$x[i_low_equal])

  i_high_equal <- findInterval(intervals$`97.5`,
                               dens$x)  # high_equal is between x[i_high_equal]
                                      # and x[i_high_equal + 1]  # BUT THINK MORE
  y_high_equal_interp <- dens$y[i_high_equal] +
    (dens$y[i_high_equal + 1] - dens$y[i_high_equal])/(dens$x[i_high_equal + 1] - dens$x[i_high_equal]) * # slope
    (intervals$`97.5` - dens$x[i_high_equal])

  # HDI
  i_low_hdi <- findInterval(intervals$hdi_lower,
                            dens$x)  # low is between dens$x[i_low] and dens$x[i_low + 1]
  y_low_hdi_interp <- dens$y[i_low_hdi] +
    (dens$y[i_low_hdi + 1] - dens$y[i_low_hdi])/(dens$x[i_low_hdi + 1] - dens$x[i_low_hdi]) * # slope
    (intervals$hdi_lower - dens$x[i_low_hdi])

  i_high_hdi <- findInterval(intervals$hdi_upper,
                             dens$x)  # high_hdi is between x[i_high_hdi]
                                      # and x[i_high_hdi + 1]  # BUT THINK MORE
  y_high_hdi_interp <- dens$y[i_high_hdi] +
    (dens$y[i_high_hdi + 1] - dens$y[i_high_hdi])/(dens$x[i_high_hdi + 1] - dens$x[i_high_hdi]) *
    (intervals$hdi_upper - dens$x[i_high_hdi])

  # OR maybe assume the low value is more accurate. Strange that these are
  # somewhat different:
  # y_low
  # y_high
  # Should presumably be equal
  res <- dplyr::mutate(intervals,
                       i_low_equal = i_low_equal,
                       y_low_equal_interp = y_low_equal_interp,
                       i_high_equal = i_high_equal,
                       y_high_equal_interp = y_high_equal_interp,
                       i_low_hdi = i_low_hdi,
                       y_low_hdi_interp = y_low_hdi_interp,
                       i_high_hdi = i_high_hdi,
                       y_high_hdi_interp = y_high_hdi_interp)

  return(list(intervals = res,
              density = dens))
}
