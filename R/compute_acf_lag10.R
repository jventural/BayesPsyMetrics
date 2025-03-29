compute_acf_lag10 <- function(fit_object) {
  # Extraer la matriz de muestras MCMC del objeto fit
  posterior_draws <- as.matrix(blavInspect(fit_object, "mcmc"))

  # Calcular la autocorrelación en lag 10 para cada parámetro
  ac_lag10 <- apply(posterior_draws, 2, function(x) {
    acf_out <- acf(x, lag.max = 10, plot = FALSE)
    acf_out$acf[11]  # Valor en el lag 10 (índice 11, ya que el lag 0 es el primer valor)
  })

  return(ac_lag10)
}
