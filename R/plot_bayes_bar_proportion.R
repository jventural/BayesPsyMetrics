plot_bayes_bar_proportion <- function(fit) {

  # Lista de paquetes necesarios
  paquetes <- c("dplyr", "tidyr", "ggplot2", "scales")

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
  library(scales)

  # Generar el gráfico
  blavaan::standardizedposterior(fit) %>%
    abs() %>%
    as_tibble() %>%
    select(contains("=~")) %>%
    pivot_longer(cols = everything(), names_to = "Ítem", values_to = "Valor") %>%
    separate(Ítem, into = c("Factor", "Items"), sep = "=~") %>%
    # Clasificamos cada draw según si es > 0.50 o no
    mutate(Destacado = ifelse(Valor > 0.50, "Mayor a 0.50", "Menor o igual a 0.50")) %>%
    # Contamos cuántas veces cada (Factor, Items, Destacado) aparece
    group_by(Factor, Items, Destacado) %>%
    summarise(n = n(), .groups = "drop") %>%
    # Calculamos la proporción dentro de cada (Factor, Items)
    group_by(Factor, Items) %>%
    mutate(prob = n / sum(n)) %>%
    ungroup() %>%
    ggplot(aes(x = Items, y = prob, fill = Destacado)) +
    # geom_col() por defecto apila las barras
    geom_col() +
    # Etiquetas de porcentaje sobre cada sección apilada
    geom_text(aes(label = scales::percent(prob, accuracy = 1)),
              position = position_stack(vjust = 0.5),
              color = "black", size = 2.5) +
    # Eje Y como porcentaje
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c("Mayor a 0.50" = "skyblue",
                                 "Menor o igual a 0.50" = "gray")) +
    facet_wrap(~ Factor, scales = "free_x") +
    theme_minimal() +
    labs(title = "",
         x = "Items",
         y = "Proporción de draws",
         fill = "Criterio") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
