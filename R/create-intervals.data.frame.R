##' Calculate ETIs and HDIs for a data frame of quantities.
##'
##' For a data frame of MCMC (or other) samples as rows and some quantity
##' given in each named column, calculate the ETI, HDI, and kernel density for
##' each column, with `create_intervals()`.
##'

##' TODO `n=1e5` is hardwired in here
##'
##' @param dat_mcmc
##' @param ... arguments to pass to `create_intervals()` and then `density()`
##'   TODO test changing from.
##' @return list TODO object of `res_all_years` which contains `year` plus
##'   `intervals` and `dens` for that year, plus `intervals_all_years` which
##'   contains all the intervals in one tibble with `year` as the first column.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' res <- create_intervals.data.frame(hake_recruitment_mcmc)   # this works,
##'   next doesn't yet
##' res <- create_intervals(hake_recruitment_mcmc)
##' res
##' }
create_intervals.data.frame <- function(dat_mcmc,
                                        ...){
  res_all <- list()
  intervals_all <- tibble::tibble()

  for(i in 1:ncol(dat_mcmc)){
    name = names(dat_mcmc)[i]

    values <- dplyr::pull(dat_mcmc,
                          name)         # Full MCMC values for that
                                        # column, numeric vector

    values <- values[!is.na(values)]    # Remove NA's, useful for sample size analysis

    res <- create_intervals(values,
                            ...)

    res_all[[i]] <- res
    res_all[[i]]$name <- name

    intervals_all <- rbind(
      intervals_all,
      cbind("quantity" = name,
            res$intervals))
  }
  intervals_all <- tibble::as_tibble(intervals_all)

  return(list(res_all = res_all,
              intervals_all = intervals_all))
}
