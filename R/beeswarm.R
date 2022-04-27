beeswarm_prepare <- function(shap, original_data) {
  stopifnot(nrow(shap) == nrow(original_data))
  
  # TODO support this only for numeric / integer or factors
  # throw error, when date or character
  
  # prepare shapley values for plotting
  shap_vals <- shap %>%
    tibble::as_tibble()
  
  shap_order <- sapply(
    X = shap_vals,
    FUN = function(x) {
      mean(abs(x))
    },
    simplify = TRUE,
    USE.NAMES = TRUE
  ) %>%
    sort(decreasing = TRUE)
  
  shap_vals$shap_id <- 1:nrow(shap)
  
  # prepare original data for plotting
  num_vars <- sapply(boston, is.numeric)
  cat_vars <- colnames(boston)[!num_vars]
  
  # make factors to numeric
  if (length(cat_vars) > 0) {
    boston <- boston %>%
      dplyr::mutate_all(utils::type.convert)
  }
  
  # scale original data
  original_data_scaled <- boston %>%
    scale()
  # create id
  original_data_scaled$shap_id <- 1:nrow(original_data)
  
}

# # importance plot
# 
# plt_imp <- shap_order |>
#   sort() |>
#   data.table::as.data.table(keep.rownames = TRUE)
# plt_imp[, ("V1") := factor(get("V1"), levels = plt_imp$V1)]
# plt_imp |>
#   ggplot2::ggplot(mapping = ggplot2::aes_string(x = "V1", y = "V2")) +
#   ggplot2::geom_col() +
#   ggplot2::coord_flip() +
#   ggplot2::labs(title = "xgboost", y = "mean(|shapley value|)", x = ggplot2::element_blank())



plt_dat <- shap_vals |>
  data.table::melt.data.table(
    id.vars = "id"
  ) |>
  data.table::merge.data.table(
    y = data.table::melt.data.table(
      data = ml_dat_test,
      id.vars = "id"
    ),
    by = c("id", "variable"),
    all.y = TRUE,
    suffixes = c("_shap", "_original")
  ) |>
  na.omit()

plt_dat$variable <- factor(plt_dat$variable, levels = rev(names(shap_order)))

plt_dat |>
  ggplot2::ggplot(mapping = ggplot2::aes_string(
    x = "variable", y = "value_shap", color = "value_original"
  )) +
  ggplot2::coord_flip() +
  ggplot2::geom_point(position = ggplot2::position_jitter()) +
  #ggforce::geom_sina() +
  viridis::scale_color_viridis(
    option = "inferno",
    breaks = c(min(plt_dat$value_original), max(plt_dat$value_original)),
    labels = c("Low", "High")) +
  ggplot2::labs(title = "xgboost", y = "shapley value", x = ggplot2::element_blank()) +
  ggplot2::theme(legend.title = ggplot2::element_text(angle = 90)) +
  ggplot2::guides(color = ggplot2::guide_colorbar(
    title = "Feature value",
    title.position = "right",
    title.hjust = 0.5
  ))