\name{plot_bayes_indices}
\alias{plot_bayes_indices}
\title{Plot Histograms of Bayesian Fit Indices (BRMSEA and BCFI)}
\description{
This function creates histograms for the fit indices extracted from a Bayesian model fit object (bfit) obtained via the \code{blavaan} package. It specifically extracts the BRMSEA and BCFI indices, reshapes the data into a tidy tibble, and then generates faceted histograms for each index using \code{ggplot2}. Users can customize the colors of the BRMSEA and BCFI histograms via the function arguments.
}
\usage{
plot_bayes_indices(bfit, col_brmsea = "#0072B2", col_bcfI = "#D55E00")
}
\arguments{
  \item{bfit}{A Bayesian model fit object from \code{blavaan} containing fit indices in its \code{@indices} slot.}
  \item{col_brmsea}{A character string specifying the color for the BRMSEA index histogram (default is "\#0072B2").}
  \item{col_bcfI}{A character string specifying the color for the BCFI index histogram (default is "\#D55E00").}
}
\details{
The function begins by extracting the BRMSEA and BCFI indices from the \code{@indices} slot of the Bayesian model object. It then creates a tibble with these indices and reshapes it into a long format using \code{pivot_longer}. A faceted histogram is generated with each fit index displayed in its own panel. The fill colors for the histograms are controlled by the \code{col_brmsea} and \code{col_bcfI} parameters, allowing for customization of the plot aesthetics.
}
\value{
A \code{ggplot} object displaying histograms of the BRMSEA and BCFI fit indices, faceted by index. This plot can be further modified, printed, or saved using standard \code{ggplot2} functions.
}
\examples{
\dontrun{
  # Assuming 'bfit' is a valid Bayesian model fit from blavaan:
  plot_bayes_indices(bfit, col_brmsea = "\#FF0000", col_bcfI = "\#00FF00")
}
}
\author{
Professor José Ventura León
}

