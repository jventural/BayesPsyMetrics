\name{rates_responses}
\alias{rates_responses}
\title{Compute and Transform Response Frequencies into Percentages}
\description{
This function calculates response frequencies for a given dataset using \code{psych::response.frequencies}, converts the output into a tibble, and then transforms the specified response columns into percentages. Additionally, it assigns a set of item labels to the output, ensuring a neatly formatted table of response percentages.
}
\usage{
rates_responses(data, items = c(1:5), responses = c("1", "2", "3"))
}
\arguments{
\item{data}{A data frame containing the variables for which the response frequencies are to be calculated.}
\item{items}{A numeric vector specifying the item labels (e.g., \code{1:5}) to be assigned in the output.}
\item{responses}{A character vector indicating the names of the columns representing response proportions; these values are multiplied by 100 to express percentages.}
}
\details{
The function performs the following steps:
\enumerate{
\item Computes response frequencies using \code{psych::response.frequencies}.
\item Converts the result into a tibble and removes the \code{miss} column.
\item Adds a new column \code{Items} populated with the provided item labels.
\item Multiplies the specified response columns by 100 to convert proportions into percentages.
\item Rounds all numeric columns to two decimal places.
}
The resulting tibble provides a clear summary of the response distributions for the selected items.
}
\value{
A tibble containing the transformed response frequencies (in percentages) along with an \code{Items} column reflecting the provided item labels.
}
\examples{
\dontrun{

Example usage with a dataset 'my_data', 5 items, and response columns "1", "2", "3", "4":
df_preliminar <- rates_responses(data = my_data, items = 1:5, responses = c("1", "2", "3", "4"))
print(df_preliminar)
}
}
\author{
Professor José Ventura León
}

