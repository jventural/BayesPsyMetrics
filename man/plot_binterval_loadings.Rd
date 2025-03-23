\name{plot_binterval_loadings}
\alias{plot_binterval_loadings}
\title{Plot Bayesian Factor Loading Intervals from blavaan Posterior Distributions}
\description{
This function extracts standardized posterior loadings from a Bayesian model fitted with the \code{blavaan} package and calculates credible intervals (2.5\% - 97.5\%) for each item. It then produces a \code{ggplot} displaying these intervals as horizontal error bars, along with a point to indicate the mean loading and a dashed reference line at x=0. The plot allows a quick visual assessment of the distribution and uncertainty of the loadings across items.
}
\usage{
plot_binterval_loadings(fit)
}
\arguments{
  \item{fit}{A fitted Bayesian model object from \code{blavaan}. It must allow the extraction of standardized posterior loadings via \code{blavaan::standardizedposterior}.}
}
\details{
The function begins by extracting the standardized posterior loadings and converting them to absolute values. Next, it reshapes the data into a tidy format (pivoting from wide to long) and groups by item to compute the mean and the 2.5\% and 97.5\% quantiles of the posterior distribution. After splitting the item identifier (originally formatted as \code{"Factor=~Item"}) into separate \code{Factor} and \code{Items} columns, it uses \code{geom_errorbarh} to create horizontal intervals (bigotes) for each item, plus a point for the mean. A dashed vertical line is added at x=0 for reference. This provides a concise overview of the central tendency and variability of the loadings across items and factors.
}
\value{
A \code{ggplot} object displaying horizontal intervals for the posterior loadings. Each interval shows the mean (as a point) and the 95\% credible interval (as error bars). The vertical reference line at x=0 aids in identifying loadings that are close to or differ substantially from zero.
}
\examples{
\dontrun{
  # Example usage (assuming 'fit' is a valid Bayesian model from blavaan):
  plot_binterval_loadings(fit)
}
}
\author{
Professor José Ventura León
}

