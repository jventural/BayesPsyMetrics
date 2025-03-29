posterior_IC <- function(fit) {
  require(dplyr)
  require(tidyr)
  require(blavaan)

  blavaan::standardizedposterior(fit) %>%
    abs() %>%
    as_tibble() %>%
    select(contains("=~")) %>%
    pivot_longer(cols = everything(),
                 names_to = "Ítem", values_to = "Valor") %>%
    group_by(Ítem) %>%
    summarise(
      media = mean(Valor),
      sd = sd(Valor),
      lower = quantile(Valor, 0.025),
      upper = quantile(Valor, 0.975)
    ) %>%
    separate(Ítem, into = c("Factor", "Items"), sep = "=~")
}
