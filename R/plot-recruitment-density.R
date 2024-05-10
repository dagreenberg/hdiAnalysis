##' Plot density function (smoothed) of MCMC recruitment for a given year, with tails shaded as specified
##'
##' @param dat_mcmc a tibble where each row represents an MCMC sample, and each
##'   column is a quantity which we want to summarise with credible intervals.
##' @param rec_intervals result of `create_intervals(dat_mcmc)`; is calculated
##'   if not supplied. May be worth supplying so it's not being repeatedly calculated.
##' @param year Which year of recruitment (age-0) to plot
##' @param type type of intervals: either `hdi` or `equal`
##' @return invisible
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot_recruitment_density()
##' plot_recruitment_density(year = 2021)
##' }
plot_recruitment_density <- function(dat_mcmc = hake_recruitment_mcmc,
                                     rec_intervals = NULL,
                                     year = 2010,
                                     type = "hdi",
                                     x_lim = c(0, 40),  # default for 2010
                                     col_main = "red",
                                     col_tail = "blue",
                                     main_title = NULL,
                                     x_lab = NULL){
  if(!(type %in% c("equal", "hdi"))){
    stop("type needs to equal or hdi.")}

# Should really filter by year first, as that's the only one we're using
  if(is.null(rec_intervals)){
    rec_intervals <- create_intervals(dat_mcmc)
  }


  if(is.null(x_lab)){
    x_lab <- paste0("Recruitment in ", year, " (billions of fish)")
  }

  dens <- density(pull(dat_mcmc,
                       as.character(year)))

  # low and high values of the interval
  if(type == "equal"){
    low <- filter(rec_intervals, name == year)$`2.5`
    high <- filter(rec_intervals, name == year)$`97.5`
    if(is.null(main_title)) {
      main_title <- "Equal-tailed 95% interval"
    }
  } else { # type == "hdi"
    low <- filter(rec_intervals, name == year)$hdi_lower
    high <- filter(rec_intervals, name == year)$hdi_upper
    if(is.null(main_title)) {
      main_title <- "HDI 95% interval"
    }
  }

  # Calculate the pdf value at low and high x values, have to interpolate.
  # Don't need to for HDI, since values will be exact x values, which I hadn't
  # realised when first doing this. (Need to check about 2.5 and 97.5).
  # And since intervals are considered [ , ) and HDI intervals are actually
  # bigger than the 'true' HDI interval, my original interpolation was actually,
  # I think, just selecting the i_high+1 value by mistake. See .Rmd for
  # thoughts.
  # Actually no, dens is a smoothed version. Think I just need to go the other
  # way for the high value??

  i_low <- findInterval(low, dens$x)  # low is between dens$x[i_low] and dens$x[i_low + 1]
  y_low <- dens$y[i_low] + (dens$y[i_low+1] - dens$y[i_low])/(dens$x[i_low+1] - dens$x[i_low]) *
           (low - dens$x[i_low])

  i_high <- findInterval(high, dens$x)  # high is between x[i_high] and x[i_high + 1]
  y_high <- dens$y[i_high] + (dens$y[i_high+1] - dens$y[i_high])/(dens$x[i_high+1] - dens$x[i_high]) *
           (high - dens$x[i_high])

  # OR maybe assume the low value is more accurate. Strange that these are
  # somewhat different:
  # y_low
  # y_high
  # Should presumably be equal


  plot(dens,
       xlab = x_lab,
       lwd = 2,
       xlim = x_lim,
       main = main_title)


  # Full distribution
  polygon(dens,
          col = col_main,
          main = "")

  # Low tail
  polygon(c(dens$x[dens$x <= low], low, low),
          c(dens$y[dens$x <= low], y_low, 0),
          col = col_tail,
          border = col_tail,
          main = "")

  # High tail
  polygon(c(high, dens$x[dens$x >= high], high),
          c(y_high, dens$y[dens$x >= high], 0),
          col = col_tail,
          border = col_tail,
          main = "")

  # Make an if once figured out:
  abline(h = y_low)

  # Prob remove this once fixed, just easier to debug with values in
  return(list(dens = dens,
              low = low,
              high = high,
              i_low = i_low,
              y_low = y_low,
              i_high = i_high,
              y_high = y_high))
}
