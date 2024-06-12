##' Calll from plot.intervals_density_list()
##'  <desc>
##'
##' @param obj
##' @param type
##' @param quantity
##' @param mfrow
##' @param ...
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @ontrun{
##' @
##' @}
plot_densities <- function(obj,
                           type,
                           quantity = NULL,
                           mfrow = c(2, 2),
                           ...){

  # plot all quantities, esp for vignette since html
  if(is.null(quantity)){
    quantity <- obj$intervals_all$quantity
  }

  par(mfrow = mfrow)

  for(i in 1:length(quantity)){
    this_quantity <- quantity[i]

    this_quantity_i <- which(obj$intervals_all$quantity == this_quantity)
    #if(this_quantity != quantity[i]){
    #  stop("quantity input not consistent with obj$intervals_all$quantity; fix code")
    #}   # think not needed now I've done it properly

    # dat_mcmc_this_quant <- obj$res_all[[i]]

    #results_index <- which(years_in_results == years[i])

    # This calls plot.intervals_density()
    plot(res_all_years$res_all[[this_quantity_i]],
         # main_title = as.character(this_quantity),
         # main_title_include = TRUE,
         type = type,
         ...)
    mtext(this_quantity,
          side = 3,
          adj = 0,
          cex = 0.7,
          line = 0.3) # TODO Add to function, cex is indpt of par(c
  }
}
