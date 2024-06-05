##' Plot Figure 1.
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
                     width = 114/25.4){

 pdf(file = file,
            height = height,
            width = width,
            paper="special")

  par(mai = c(0.32, 0.3, 0.15, 0.12),
      mfrow = c(2,1),
      mgp = c(1.6,0.5,0),
      cex = 0.6)

  plot_density(type = "equal",
               ylim = c(0, 0.11),
               interval_arrows = TRUE)

  mtext("A. Equal-tailed interval", side = 3, adj = 0, cex = 0.7,
        line = 0.3) # TODO Add to function, cex is indpt of par(cex)

  plot_density(type = "hdi",
               ylim = c(0, 0.11),
             interval_arrows = TRUE)

  mtext("B. Highest density interval", side = 3, adj = 0, cex = 0.7,
        line = 0.3) # TODO Add to function

  dev.off()
}
