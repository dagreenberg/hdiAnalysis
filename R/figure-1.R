##' Plot Figure 1 for a specific size.
##'  <desc>
##'
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
figure_1 <- function(file = "figure-1.pdf",
                     height = 4,
                     width = 114/25.4,
                     xlim = c(0, 40),
                     xlab = "Recruitment (billions of fish)",
                     ...){

  res_2021 <- create_intervals(rec_2021)

  pdf(file = file,
      height = height,
      width = width,
      paper="special")

  par(mai = c(0.32, 0.3, 0.15, 0.12),
      mfrow = c(2,1),
      mgp = c(1.6,0.5,0),
      cex = 0.6)

   plot(res_2021,
       type = "eti",
       xlim = xlim,
       ylim = c(0, 0.11),
       interval_arrows = TRUE,
       xlab = xlab,
       arrowhead_gap = 0.1,
       ...)

  mtext("A. Equal-tailed interval", side = 3, adj = 0, cex = 0.7,
        line = 0.3) # TODO Add to function, cex is indpt of par(cex)

  plot(res_2021,
       type = "hdi",
       xlim = xlim,
       ylim = c(0, 0.11),
       interval_arrows = TRUE,
       xlab = xlab,
       arrowhead_gap = 0.1,
       ...)

  mtext("B. Highest density interval", side = 3, adj = 0, cex = 0.7,
        line = 0.3) # TODO Add to function

  dev.off()
}
