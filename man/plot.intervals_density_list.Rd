% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.intervals_density_list.R
\name{plot.intervals_density_list}
\alias{plot.intervals_density_list}
\title{Plot a time series of intervals from multiple samples, as calculated from
\code{create_intervals()} applied to a data frame object.}
\usage{
\method{plot}{intervals_density_list}(
  obj,
  type = "comparison",
  inc = 0.15,
  x_tick_extra_years = 20,
  add_big_ticks_x = TRUE,
  start_big_ticks_x = NULL,
  eti_bar_col = "blue",
  hdi_bar_col = "red",
  add_line_at_0.4 = FALSE,
  add_line_at_0.4_col = "darkgreen",
  add_line_at_0.4_lty = 5,
  ylim = NULL,
  y_tick_start = 0,
  y_tick_end = NULL,
  y_tick_by = 1,
  pch = 20,
  cex = 0.8,
  inset = c(0, -0.02),
  add_legend = TRUE,
  leg_loc = "topright",
  join_intervals = FALSE,
  arrowhead_length = 0.15,
  mfrow = NULL,
  ...
)
}
\arguments{
\item{obj}{a \code{pacea_recruitment} object, which is a time series. Function
will run on other objects (not give an error) but is not tested on those.}

\item{add_line_at_0.4}{logical whether to add a line at 0.4, specifically
for Figure 2B, also adds the 'Now' and 'Projections' text}

\item{add_line_at_0.4_col}{colour for 0.4 line}

\item{add_line_at_0.4_lty}{linetype for 0.4 line}

\item{...}{further options passed onto \code{plot.default()}}

\item{value}{the column to plot if no uncertainties, or what to plot as dots
if showing uncertainties (likely always \code{median})}

\item{style}{\code{no_uncertainty} for plain time series without uncertainty,
gets overridden to have uncertainty bars if \code{low} and \code{high} are columns
of \code{obj}}

\item{uncertainty_bar_col}{colour for uncertainty bars for certain types of
plot (e.g. estimated fish recruitment)}

\item{y_max}{maximum y value for certain types of plot (use this if you get
an error when specifying \code{ylim})}

\item{add_line_at_1}{whether to add a horizontal line at 1 (only sensible for scaled recruitments)}

\item{add_line_at_1_col}{colour for line at 1}

\item{add_line_at_1_lty}{line type of line at 1}
}
\value{
plot of the time series as median with bars showing uncertainty (if
\code{low} and \code{high} are columns of `obj) to the current device; returns nothing.
}
\description{
Default plot is to show show ETIs, HDIs, or both (maybe only works for a
time series?), as saved in the \code{intervals_all} component of the list from
\code{create_intervals()}. See example and vignette. Also option to plot the density for each quantity (years in
our recruitment example).
}
\details{
TODO adapting from \code{plot_series} which I can delete once finalised.

Adapted from \code{pacea::plot.pacea_recruitment()}.

TODO Temporal plot for a time series (of class
\code{pacea_recruitment}) object. The \code{style} option here (unlike for
\code{plot.pacea_index()} defaults to \code{no_uncertainty} and gets changed to
\code{uncertainty} if \code{low} and \code{high} are columns of \code{obj}.
}
\examples{
\dontrun{

res_all_years <- create_intervals(dplyr::select(hake_recruitment_mcmc,
                                                         -"Virgin"))     # TODO
plot(res_all_years)
}
}
\author{
Andrew Edwards
}
