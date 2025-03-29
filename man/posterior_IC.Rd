\name{posterior_summary}
\alias{posterior_summary}
\title{Summarize Standardized Posterior Loadings from a Bayesian Model}
\description{
This function computes a summary of the absolute standardized posterior loadings obtained from a \code{blavaan} model fit. It selects only the factor loading parameters (those whose names contain "\code{=~}"), transforms the data into a tibble, and calculates summary statistics (mean, standard deviation, and 95\% credible interval) for each item. Finally, the function separates the combined parameter name into \code{Factor} and \code{Items}.
}
\usage{
posterior_summary(fit)
}
\arguments{
  \item{fit}{An object of class \code{blavaan} (or \code{lavaan}) representing a fitted Bayesian latent variable model.}
}
\details{
The function performs the following steps:
\enumerate{
  \item Computes the absolute value of the standardized posterior loadings using \code{blavaan::standardizedposterior}.
  \item Converts the resulting matrix into a tibble.
  \item Selects the columns whose names contain "\code{=~}", corresponding to factor loadings.
  \item Transforms the data into long format, where each row represents one parameter's posterior sample.
  \item For each item, calculates the mean, standard deviation, and the 2.5th and 97.5th percentiles (forming a 95\% credible interval).
  \item Separates the parameter name into two components: \code{Factor} and \code{Items} (using "\code{=~}" as the delimiter).
}
The resulting tibble provides a neat summary of the posterior distribution for the factor loadings.
}
\value{
A tibble with the following columns:
\itemize{
  \item \code{Factor}: The latent factor name.
  \item \code{Items}: The observed variable (item) name.
  \item \code{media}: The posterior mean of the absolute standardized loading.
  \item \code{sd}: The posterior standard deviation.
  \item \code{lower}: The lower bound of the 95\% credible interval.
  \item \code{upper}: The upper bound of the 95\% credible interval.
}
}
\examples{
\dontrun{
  # Fit a Bayesian confirmatory factor analysis model using bcfa
  model1 <- "F1 =~ RSE1 + RSE2 + RSE3 + RSE4 + RSE5 + RSE6 + RSE7 + RSE8 + RSE9 + RSE10"
  fit <- bcfa(model1, data = my_data, n.chains = 3, burnin = 1000, sample = 1000, seed = 12345)

  # Obtain the summary of the standardized posterior loadings
  summary_tbl <- posterior_summary(fit)
  print(summary_tbl)
}
}
\author{
Professor José Ventura León
}
