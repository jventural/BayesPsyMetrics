\name{plot_bayes_bar_proportion}
\alias{plot_bayes_bar_proportion}
\title{Plot Proportions of Posterior Draws Surpassing a Threshold in Bayesian Factor Loadings}
\description{
This function computes and displays the proportion of posterior samples (from a Bayesian factor model fitted with the \code{blavaan} package) that exceed a specified threshold (by default 0.50). It uses stacked bars to represent how many draws per item are above or below (or equal to) the threshold. The function transforms the posterior loadings into a tidy format, classifies each draw as either "Mayor a 0.50" or "Menor o igual a 0.50," calculates the proportion of draws in each category, and then plots these proportions in stacked bar charts.
}
\usage{
plot_bayes_bar_proportion(fit)
}
\arguments{
  \item{fit}{A fitted Bayesian model object from \code{blavaan}. It must allow the extraction of standardized posterior loadings via \code{blavaan::standardizedposterior}.}
}
\details{
The function first extracts the standardized posterior loadings, converts them to absolute values, and pivots them from wide to long format. It splits the combined factor-item labels (originally \code{"Factor=~Item"}) into separate columns for factor and item. Each draw is then labeled according to whether its loading exceeds 0.50 or not. The total draws per item are counted and converted to a proportion for each category ("Mayor a 0.50" vs "Menor o igual a 0.50"). The resulting proportions are visualized in a stacked bar chart, with each bar subdivided according to the proportion of draws in each category. The y-axis is formatted as a percentage for clarity, and separate facets are created for each factor.
}
\value{
A \code{ggplot} object displaying the stacked bar charts of proportions per item, with each bar indicating how many draws are above vs below the threshold (by default 0.50). This visualization allows quick identification of items that frequently exceed the threshold in the posterior draws.
}
\examples{
\dontrun{
  # Assuming 'fit' is a valid Bayesian model from blavaan:
  plot_bayes_bar_proportion(fit)
}
}
\author{
Professor José Ventura León
}

