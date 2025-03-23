plot_binterval_loadings <- function(fit) {

  # Paquetes necesarios
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

  # Generar el gráfico
  blavaan::standardizedposterior(fit) %>%
    abs() %>%
    as_tibble() %>%
    select(contains("=~")) %>%
    pivot_longer(cols = everything(), names_to = "Ítem", values_to = "Valor") %>%
    group_by(Ítem) %>%
    summarise(
      media = mean(Valor),
      lower = quantile(Valor, 0.025),
      upper = quantile(Valor, 0.975)
    ) %>%
    separate(Ítem, into = c("Factor", "Items"), sep = "=~") %>%
    ggplot(aes(y = Items, x = media)) +
    # Dibuja la barra horizontal (intervalo) con “bigotes” verticales en los extremos
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   height = 0.3,   # Ajusta el alto del "bigote" vertical
                   color = "grey32") +
    # Agrega el punto de la media
    geom_point(size = 3, color = "grey32", fill = "grey32", shape = 21) +
    # Línea vertical de referencia en x=0
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    theme_minimal() +
    labs(title = "",
         x = "Loadings",
         y = "Item") +
    theme(axis.text.y = element_text(size = 10))
}
