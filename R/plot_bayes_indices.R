plot_bayes_indices <- function(bfit, col_brmsea = "#0072B2", col_bcfI = "#D55E00") {

  # Lista de paquetes requeridos
  paquetes <- c("tibble", "tidyr", "ggplot2")
  for (pkg in paquetes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }

  # Cargar los paquetes
  library(tibble)
  library(tidyr)
  library(ggplot2)

  # Preparar los datos
  datos_indices <- tibble(
    BRMSEA = bfit@indices$BRMSEA,
    BCFI = bfit@indices$BCFI
  ) %>%
    pivot_longer(cols = everything(), names_to = "Índice", values_to = "Valor")

  # Generar el gráfico
  grafico_indices <- ggplot(datos_indices, aes(x = Valor, fill = Índice)) +
    geom_histogram(bins = 15, color = "white", show.legend = FALSE) +
    facet_wrap(~ Índice, scales = "free") +
    scale_fill_manual(values = c("BRMSEA" = col_brmsea, "BCFI" = col_bcfI)) +
    theme_minimal() +
    labs(title = "",
         x = "Value",
         y = "")

  return(grafico_indices)
}
