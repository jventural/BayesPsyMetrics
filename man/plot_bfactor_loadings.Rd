\name{plot_bfactor_loadings}
\alias{plot_bfactor_loadings}
\title{Plot Boxplots of Bayesian Factor Loadings from blavaan Posterior Distributions}
\description{
This function extracts standardized posterior loadings from a Bayesian model fitted
with the \code{blavaan} package and creates boxplots to summarize the distribution of
these loadings. The loadings are converted to their absolute values, and the resulting
matrix is transformed into a tidy tibble by pivoting from wide to long format.
The column names, originally in the form \code{"Factor=~Item"}, are split into two
separate columns. A faceted \code{ggplot2} boxplot is then produced with items on the
x-axis and loadings on the y-axis, allowing for a clear visual inspection.
}
\usage{
plot_bfactor_loadings(fit)
}
\arguments{
  \item{fit}{A fitted Bayesian model object from \code{blavaan}. It must allow for the
  extraction of standardized posterior loadings via \code{blavaan::standardizedposterior}.}
}
\details{
The function begins by extracting the standardized posterior loadings and taking their
absolute values. It then converts the resulting matrix to a tibble, pivots the data from
wide to long format, and splits the original column names (formatted as \code{"Factor=~Item"})
into two separate columns: \code{Factor} and \code{Item}. The boxplot is generated with
the items on the x-axis and the loadings on the y-axis, using \code{facet_grid} to create
separate panels for each factor. The y-axis is scaled from 0 to 1 with breaks at every 0.10.
}
\value{
A \code{ggplot} object displaying the boxplots of the factor loadings. This plot can be
printed, further modified, or saved using standard \code{ggplot2} functions.
}
\examples{
plot_bfactor_loadings(fit)
}
\author{
Professor José Ventura León
}

# Función en R:
plot_bfactor_loadings <- function(fit) {

  # Lista de paquetes requeridos
  paquetes <- c("dplyr", "tidyr", "ggplot2")

  # Instalar los paquetes que no estén instalados
  for (pkg in paquetes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }

  # Cargar los paquetes
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  # Procesar los datos y generar el gráfico
  blavaan::standardizedposterior(fit) %>%
    abs() %>%
    as_tibble() %>%
    select(contains("=~")) %>%
    pivot_longer(cols = everything(), names_to = "Ítem", values_to = "Valor") %>%
    separate(Ítem, into = c("Factor", "Item"), sep = "=~") %>%
    ggplot(aes(x = Item, y = Valor, fill = Item)) +
      geom_boxplot() +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.10)) +
      facet_grid(~Factor, scales = "free_x") +
      theme_minimal() +
      labs(title = "",
           x = "Item",
           y = "Loading") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
}
