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

  # Procesar datos y generar el gráfico
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
