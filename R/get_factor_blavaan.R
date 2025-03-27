get_factor_blavaan <- function(fit_model, credMass = 0.95) {
  # Asegúrate de que las dependencias están cargadas
  if (!require(data.table)) install.packages("data.table")
  library(data.table)

  # Extraer el estadístico de convergencia PSRF para las cargas factoriales
  psrf <- blavInspect(fit_model, what = "psrf")

  # Extraer las estimaciones posteriores estandarizadas
  stpost <- as.data.frame(standardizedPosterior(fit_model))

  # Lista para almacenar los resultados
  results_list <- list()

  # Bucle a través de las cargas factoriales (columnas que contengan "=~")
  for (i in grep("=~", names(stpost), value = TRUE)) {
    # Usar una expresión regular para extraer el factor y el ítem
    # Se asume que el formato es "F#=~RSE#" (por ejemplo, "F1=~RSE1")
    factor_item <- sub("^(F\\d+)=~(RSE\\d+)$", "\\1-\\2", i)

    posterior_mean <- mean(stpost[[i]])
    hdi_values <- HDInterval::hdi(stpost[[i]], credMass = credMass)
    hdi_formatted <- sprintf("[%0.3f, %0.3f]", hdi_values[1], hdi_values[2])

    # Extraer el valor de PSRF correspondiente
    psrf_value <- psrf[i]
    psrf_display <- ifelse(!is.na(psrf_value), sprintf("%.3f", psrf_value), "–")

    # Añadir a la lista de resultados
    results_list[[length(results_list) + 1]] <- list(
      Factor = factor_item,
      PosteriorMean = posterior_mean,
      CredibleInterval = hdi_formatted,
      PSRF = psrf_display
    )
  }

  # Convertir la lista en un data frame usando data.table
  results_df <- data.table::rbindlist(results_list, fill = TRUE)

  # Si la primera entrada de PSRF es NA, asignar un guión
  if (nrow(results_df) > 0 && is.na(psrf[1])) {
    results_df$PSRF[1] <- "–"
  }

  return(results_df)
}
