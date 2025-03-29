\name{compute_acf_lag10}
\alias{compute_acf_lag10}
\title{Compute Autocorrelation at Lag 10 for Each Parameter from a Bayesian Model}
\description{
This function extracts the MCMC samples from a \code{blavaan} (or \code{lavaan}) model fit and computes the autocorrelation at lag 10 for each parameter. It returns a numeric vector where each element corresponds to the autocorrelation value at lag 10.
}
\usage{
compute_acf_lag10(fit)
}
\arguments{
  \item{fit}{An object of class \code{blavaan} (or \code{lavaan}) representing a fitted Bayesian latent variable model.}
}
\details{
The function performs the following steps:
\enumerate{
  \item Extracts the MCMC samples from the model using \code{blavInspect(fit, "mcmc")}.
  \item For each parameter, computes the autocorrelation function (ACF) with a maximum lag of 10 using \code{acf()}.
  \item Retrieves the autocorrelation value at lag 10 (the 11th element, since lag 0 is the first value).
}
}
\value{
A numeric vector containing the autocorrelation at lag 10 for each parameter.
}
\examples{
\dontrun{
  # Fit a Bayesian model using bcfa
  model1 <- "F1 =~ RSE1 + RSE2 + RSE3 + RSE4 + RSE5 + RSE6 + RSE7 + RSE8 + RSE9 + RSE10"
  fit <- bcfa(model1, data = my_data, n.chains = 3, burnin = 1000, sample = 1000, seed = 12345)

  # Compute autocorrelation at lag 10 for each parameter
  ac_lag10 <- compute_acf_lag10(fit)
  print(ac_lag10)
}
}
\author{
Professor José Ventura León
}
