\name{plot_bhist_loadings}
\alias{plot_bhist_loadings}
\title{Plot Histograms of Bayesian Factor Loadings from blavaan Posterior Distributions}
\description{
This function extracts standardized posterior loadings from a Bayesian model fitted with the \code{blavaan} package and creates histograms to summarize the distribution of these loadings. The function converts the loadings to their absolute values, transforms the resulting matrix into a tidy tibble by pivoting from wide to long format, and separates the column names (originally formatted as \code{"Factor=~Item"}) into separate \code{Factor} and \code{Item} columns. Additionally, the items are reordered based on the numerical part of their names to ensure a natural ordering in the plots. Finally, a faceted \code{ggplot2} histogram is generated for each item.
}
\usage{
plot_bhist_loadings(fit)
}
\arguments{
  \item{fit}{A fitted Bayesian model object from \code{blavaan}. The object must allow the extraction of standardized posterior loadings via \code{blavaan::standardizedposterior}.}
}
\details{
The function first extracts the standardized posterior loadings and converts them to absolute values. The data is then transformed into a tibble and pivoted from wide to long format so that each loading is stored in a single row along with its corresponding item identifier. The original column names, formatted as \code{"Factor=~Item"}, are split into two separate columns, \code{Factor} and \code{Item}. The \code{Item} column is further reordered based on the numeric component extracted from its names using \code{forcats::fct_reorder}, ensuring that items are displayed in natural order. Finally, histograms of the loadings are created using \code{ggplot2}, with each item displayed in its own facet.
}
\value{
A \code{ggplot} object displaying histograms of the factor loadings for each item. This plot can be printed, further modified, or saved using standard \code{ggplot2} functions.
}
\examples{
\dontrun{
  # Assuming 'fit' is a valid Bayesian model fit from blavaan:
  plot_bhist_loadings(fit)
}
}
\author{
Professor José Ventura León
}

