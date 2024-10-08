% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-intervals.numeric.R
\name{create_intervals.numeric}
\alias{create_intervals.numeric}
\title{For a vector of numeric values, calculate the ETI and HDI}
\usage{
\method{create_intervals}{numeric}(
  dat,
  density = FALSE,
  credibility = 0.95,
  from = 0,
  n = 1e+05,
  tolerance = 0.01,
  allow_hdi_zero = FALSE,
  ...
)
}
\arguments{
\item{dat}{numeric vector of values, such as MCMC samples for a quantity}

\item{density}{if TRUE then use the density approach for the HDI
calculation, rather than the \code{hdi()} default of just the sample values. If
FALSE (the default) then the density kernel is only used to estimate the y
values of the pdf at specified points.}

\item{credibility}{numeric value between 0 and 1 specifying the interval to
be specified (0.95 for 95\%, 0.90 for 90\%, etc.)}

\item{from}{the left-most point of the grid at which the density is to be
estimated; if NULL then the default in \code{density()} will be used, which is
'cut * bw' outside of 'min(x)' (see \code{?density}), and can fall below 0.}

\item{n}{the number of equally spaced points at which the density is
to be estimated, to be passed onto \code{density()}. We found the \code{density()}
default of 512 to give inaccurate results, so set a higher default here as
1e05 (\code{?density} advises to use powers of 2 as the value gets rounded up
anyway, but we found this not to be the case). Changing \code{n} changes the
resolution of the density kernel but not the wiggliness.}

\item{tolerance}{amount (as a proportion) that the relative difference
between \code{y_hdi_lower} and \code{y_hdi_upper} can be, calculated as their
absolute difference divided by their mean. If the calculation is larger
than \code{tolerance} then a warning is given. Only applicable when \code{density = TRUE}. NOT USED NOW, decide if need to keep (prob (not).}

\item{allow_hdi_zero}{logical, if TRUE then allow HDI lower bound to include
zero or be negative; if FALSE (the default) then do not allow this.}

\item{...}{arguments to pass onto \code{density()}, including \code{to} which is
the right-most equivalent to \code{from}.}
}
\value{
list object of class \code{intervals_density} (such that we can plot it
with \code{plot.intervals_density()}, with objects:   TODO add new ones, plus
density object. Link
help files together.
\itemize{
\item intervals: one-row tibble with columns:
\itemize{
\item median: median of the data
\item eti_lower: lower end of the ETI
\item eti_upper: upper end of the ETI
\item hdi_lower: lower end of the HDI
\item hdi_upper: upper end of the HDI
\item width_eti: width of the ETI
\item width_hdi: width of the HDI
\item width_diff: difference in widths, how much smaller (more certain) the
HDI is than the ETI
\item i_eti_lower: index for which \code{eti_lower} is between
\code{dens$x[i_eti_lower]} and \code{dens$x[i_eti_lower + 1]}
\item y_eti_lower: linearly interpolated value based on \code{i_eti_lower}
corresponding to the density at \code{eti_lower}
\item i_eti_upper, y_eti_upper: similar to \code{...lower} but for \code{upper}
\item i_hdi_lower: index for which \code{dens$x[i_hdi_lower] = hdi_lower}. The
theoretical true value of the lower bound of HDI will lie between
\code{dens$x[i_hdi_lower - 1]} and \code{dens$x[i_hdi_lower]}, but the high \code{n} used
should make this range small enough
\item y_hdi_lower: the density at \code{dens$y[i_hdi_lower]} corresponding to \code{hdi_lower}
\item i_hdi_upper: index for which \code{dens$x[i_hdi_upper] = hdi_upper}. The
theoretical true value of the upper bound of HDI will lie between
\code{dens$x[i_hdi_upper]} and \code{dens$x[i_hdi_upper + 1]} (note the asymmetry to
\code{i_hdi_lower}), but the high \code{n} used should make this range small enough
\item y_hdi_upper: the density at \code{dens$y[i_hdi_upper]} corresponding to \code{hdi_upper}
\item hdi_height: the height of the pdf returned from \code{HDInterval::hdi()},
corresponding to either \code{y_hdi_lower} or \code{y_hdi_upper} (depending on which
is the first \code{dens$x} value to push the integrated sum of the sorted
cumulative \code{dens$y} values over \code{credibility}; see
\code{HDInterval::hdi.density()}. Is \code{NA} if \code{density = FALSE}.
\item warning: logical, if \code{TRUE} then a warning was produced during the
\code{HDInterval::hdi()} calculation. If no warning printed then this warning
was "The HDI is discontinuous but allowSplit = FALSE; the result is a
valid CrI but not HDI.", else the new warning "New type of warning in
create_intervals()." is printed and needs investigating. See
\code{plot.intervals_density()} with \code{show_discontinuity = TRUE} to plot the
discontinuities in the HDI.
\item allow_hdi_zero: logical of \code{allow_hdi_zero} used
}
}
}
\description{
For a vector of numeric values, calculate the ETI and HDI
}
\examples{
\dontrun{
create_intervals(rec_2021)   # rec_2021 is the MCMC samples for hake recruitment in 2021
}
}
\author{
Andrew Edwards
}
