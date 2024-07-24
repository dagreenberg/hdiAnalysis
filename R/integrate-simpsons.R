##' Integration of a one-dimensional kernel density estimate using Simpson's rule of integration
##'
##' Based on algorithm in Table 18.4 on p961 of Kreyszig (1993) Advanced
##' Engineering Mathematics (Seventh Edition). Also the [Composition Simpson's
##' 1/3 rule on
##' Wikipedia](https://en.wikipedia.org/wiki/Simpson's_rule#Simpson.27s_3.2F8_rule),
##' which is slightly easier notation for implementation, so we follow that here.
##'
##' Simpson's rule requires an even number of equal subintervals; if there are an
##' odd number (so even number of points, which is likely the case for density
##' calculations since the points will be a round number), then we use the
##' trapezoid rule for the final point, which is likely close to zero anyway
##' because the density kernel should be reaching far enough away from the bulk
##' of the density.
##'
##' @param dens kernel density estimate, that has `$x` as the x values with
##'   corresponding `$y` for the density estimates.
##' @param domain numeric vector of two values giving the domain over which to
##'   calculate the integral
##' @param tol numeric tolerance for checking that `dens$x` are equally spaced;
##'   increase if get an error
##' @return the value of the integral
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' integrate_simpsons(density(rnorm(8000), n = 1e05))  # n as used in default
##'   for create_intervals()
##' dens <- density(sim$values)
##' integrate_simpsons(dens)
##' }
integrate_simpsons <- function(dens,
                               domain,
                               tol = 1e-08){
  stopifnot(class(dens) == "density")

  diff_x <- diff(dens$x)
  stopifnot("Check that dens$x is equally spaced; increase `tol` if needed" =
              diff(range(diff_x)) <= tol)
browser()
  if(missing(domain)){
    # Integrate over full domain
    x_domain <- dens$x
    y_domain <- dens$y
  } else {
    stopifnot(length(domain) == 2)

    x_indices_in_domain <- which(dens$x >= domain[1] & dens$x <= domain[2])
    x_domain <- dens$x[x_indices_in_domain]
    y_domain <- dens$y[x_indices_in_domain]
    # TODO browser and do some manual checking here
  }

# TODO Add a WARNING if domain not close to exact points - actually will be for
  # our purposes since using density and domain will be some density-based value
  # probably. Actually may just need a check.

  # If an even number of points then use trapezoid rule for final one, and then
  # remove it for Simpson's rule calculation
  num_domain_points <- length(x_domain)
  if(num_domain_points %% 2 == 0){
    final_interval_x_values <- x_domain[(num_domain_points - 1):num_domain_points]
    final_interval_y_values <- y_domain[(num_domain_points - 1):num_domain_points]
    int_final_interval <- mean(final_interval_y_values) * diff(final_interval_x_values)

    # Remove final values
    x <- x_domain[-num_domain_points]
    y <- y_domain[-num_domain_points]
  } else {
    int_final_interval <- 0
    x <- x_domain
    y <- y_domain
  }

  n <- length(x) - 1       # Number of sub-intervals. i=0 in formulae
                           # corresponds to index 1 in vectors here, etc for others.
  stopifnot(n %% 2 == 0)   # n will be even

  h <- (max(x) - min(x))/ n

  # Wikipedia formula is, with integral being J:
  # J ~= h/3 * (f(x_0) + 4 * sum_{i=1}^{n/2} f(x_{2i - 1}) +
  #       2 * sum_{i=1}^{n/2-1} f(x_{2i}) + f(x_n) )

  # Index i there starts at 0, so write code here to have index starting at
  #  j=1, given R starts indexing vectors at 1 (unlike, say, Matlab).
  # Gets confusing to rewrite the equations, so just do the code

  # x_0  x_1  x_2  ...  x_n      - equations
  # x[1] x[2] x[3] ...  x[n+1]   - R vectors
  # Similarly:
  # f(x_0) f(x_1) f(x_2)  ... f(x_n)      - equations
  # y[1]   y[2]   y[3]    ...  y[n+1]

  # Components of the above formula are:
  # f(x_0)
  # y[1]

  # sum_{i=1}^{n/2} f(x_{2i - 1}) = s1 in Kreyszig notation
  # f(x_1) + f(x_3) + f(x_5) + ... f(x_{n-1})
  # y[2]   + y[4]   + y[6]   + ... y[n]
  # sum(y[even_indices])

  even_indices <- (1:(n/2))* 2      # even corresponding to the indices of vectors
  odd_indices <- even_indices[-length(even_indices)] + 1   # one fewer of them than even

  # Simple example:
  # n <- 10
  # even_indices <- (1:(n/2))* 2
  #   2  4  6  8 10
  # odd_indices <- even_indices[-length(even_indices)] + 1
  #   3 5 7 9

  # sum_{i=1}^{n/2-1} f(x_{2i}) = s2
  # f(x_2) + f(x_4) + f(x_6) + ... + f(x_{n-2})
  # y[3]   + y[5]   + y[7]   + ... + y[n-1]
  # sum(y[odd_indices])

  # f(x_n)
  # y[n+1]

  J <- h/3 * ( y[1] + 4 * sum(y[even_indices]) + 2 * sum(y[odd_indices]) +
       y[n+1]) + int_final_interval

  return(J)
}
