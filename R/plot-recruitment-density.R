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

  if(is.null(rec_intervals)){
    rec_intervals <- create_intervals(dat_mcmc)
  }


  if(is.null(x_lab)){
    x_lab <- paste0("Recruitment in ", year, " (billions of fish)")
  }

  dens <- density(pull(dat_mcmc,
                       as.character(year)))

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
  polygon(c(dens$x[dens$x <= low], low),
          c(dens$y[dens$x <= low], 0),
          col = col_tail,
          border = col_tail,
          main = "")

  # High tail
  polygon(c(dens$x[dens$x >= high], high),
          c(dens$y[dens$x >= high], 0),
          col = col_tail,
          border = col_tail,
          main = "")

  invisible()
}
