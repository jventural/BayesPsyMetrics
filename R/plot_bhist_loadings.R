plot_bhist_loadings <- function(fit) {

  # Lista de paquetes requeridos
  paquetes <- c("dplyr", "tidyr", "ggplot2", "forcats")

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
  library(forcats)

  # Transformar datos y generar el gráfico
  blavaan::standardizedposterior(fit) %>%
    abs() %>%
    as_tibble() %>%
    select(contains("=~")) %>%
    pivot_longer(cols = everything(), names_to = "Ítem", values_to = "Valor") %>%
    separate(Ítem, into = c("Factor", "Item"), sep = "=~") %>%
    mutate(Item = forcats::fct_reorder(Item,
                                       as.numeric(gsub("[^0-9]", "", as.character(Item))),
                                       .desc = FALSE)) %>%
    ggplot(aes(x = Valor)) +
    geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
    facet_wrap(~ Item, scales = "free_y") +
    theme_minimal() +
    labs(title = "",
         x = "Loadings",
         y = "Frequency")
}
