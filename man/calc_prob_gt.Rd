\name{calc_prob_gt}
\alias{calc_prob_gt}
\title{Calculate the Proportion of Posterior Draws Exceeding a Threshold for Bayesian Factor Loadings}
\description{
This function computes the proportion of posterior draws for one or more Bayesian factor loadings (obtained via \code{blavaan::standardizedposterior}) that exceed a specified threshold. If a specific item is provided, it returns the probability for that item; if the special value \code{"All"} is given as the item argument (case-insensitive), the function calculates and returns a tibble containing the probabilities for all items. Additionally, when computing for all items, the original factor-item labels (formatted as \code{"Factor=~Item"}) are split into two separate columns and the factor is renamed as \code{actor}.
}
\usage{
calc_prob_gt(fit, item, threshold = 0.50)
}
\arguments{
  \item{fit}{A fitted Bayesian model object from \code{blavaan} that supports the extraction of standardized posterior loadings via \code{blavaan::standardizedposterior}.}
  \item{item}{A character string specifying the name of the item (e.g., \code{"F1=~bif2"}) for which the proportion is to be calculated. Alternatively, use \code{"All"} to compute the proportions for all items.}
  \item{threshold}{A numeric value indicating the threshold above which a posterior draw is considered. The default value is 0.50.}
}
\details{
The function first extracts the standardized posterior loadings and converts them to absolute values. The resulting data is transformed into a tibble containing only the columns corresponding to factor loadings. When a specific item is provided, the function checks that the item exists among the columns and then computes the proportion of posterior draws exceeding the given threshold. When \code{"All"} is specified, the function computes this proportion for every item, creates a tibble with the item names and their corresponding probabilities, and then separates the item names (originally in the format \code{"Factor=~Item"}) into separate \code{Factor} and \code{Items} columns, renaming the factor column to \code{actor}.
}
\value{
Either a single numeric value representing the proportion of posterior draws exceeding the threshold for the specified item, or a tibble containing the \code{actor}, \code{Items}, and \code{probability} columns for all items when \code{"All"} is specified.
}
\examples{
\dontrun{
  # Calculate the probability that the loading for a specific item exceeds 0.80:
  prob_bif2 <- calc_prob_gt(fit, "F1=~bif2", 0.80)

  # Calculate the probabilities for all items with a threshold of 0.80:
  prob_all <- calc_prob_gt(fit, "All", 0.80)
  print(prob_all)
}
}
\author{
Professor José Ventura León
}
