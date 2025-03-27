create_null_model <- function(name_item = "RSE", items = 1:10) {
  model <- c(
    paste0(name_item, items, " ~~ ", name_item, items),
    paste0(name_item, items, " ~ 1")
  )
  return(model)
}
