calc_prob_gt <- function(fit, item, threshold = 0.50) {
  # Extrae la matriz de parámetros estandarizados y toma el valor absoluto
  df <- blavaan::standardizedposterior(fit) %>%
    abs() %>%
    as_tibble() %>%
    select(contains("=~"))

  # Si se solicita "All", calcula la probabilidad para cada ítem y separa el nombre
  if(tolower(item) == "all") {
    probs <- sapply(names(df), function(it) mean(df[[it]] > threshold))
    result <- tibble(item = names(probs), probability = probs) %>%
      separate(item, into = c("Factor", "Items"), sep = "=~") %>%
      rename(actor = Factor)
    return(result)
  } else {
    # Verifica que el ítem exista en las columnas
    if(!(item %in% names(df))) {
      stop(paste("El ítem", item, "no se encuentra en los datos"))
    }
    # Calcula la probabilidad para el ítem específico
    prob <- mean(df[[item]] > threshold)
    return(prob)
  }
}
