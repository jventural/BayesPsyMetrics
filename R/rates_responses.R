rates_responses <- function(data, items = c(1:5), responses = c("1", "2", "3")) {
  data %>%
    psych::response.frequencies() %>%
    as_tibble() %>%
    select(-miss) %>%
    mutate(Items = items) %>%
    relocate(Items) %>%
    mutate(across(all_of(responses), ~ .x * 100)) %>%
    mutate(across(where(is.numeric), round, 2))
}
