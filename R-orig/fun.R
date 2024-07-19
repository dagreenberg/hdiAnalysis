fun <- function(x,
                force_hdi_not_zero = TRUE){
  res <- x^2
  if(force_hdi_not_zero){
    res <- fun(res,
               force_hdi_not_zero = FALSE)
  }
  return(res)
}
