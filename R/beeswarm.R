#' Internal function to prepare data for beeswarm plot
#' 
#' Creates a tibble with the input data required for plotting a beeswarm plot
#' 
#' @inheritParams autoplot.explain
#' 
#' @return  A tibble created with internal function
#'   \code{beeswarm_prepare}.
#' 
#' @importFrom magrittr `%>%`
#'
beeswarm_prepare <- function(object, X, num_features) {
  stopifnot(
    nrow(object) == nrow(X),
    ifelse(is.null(num_features), TRUE, is.integer(num_features))
  )
  
  # prepare shapley values for plotting
  shap_vals <- object %>%
    tibble::as_tibble()
  original_data <- X %>%
    tibble::as_tibble()
  
  if (!is.null(num_features)) {
    feature_names <- colnames(shap_vals)[1:num_features]
    shap_vals <- shap_vals %>%
      dplyr::select(feature_names)
    original_data <- original_data %>%
      dplyr::select(feature_names)
  }
  
  
  # sort variable names by mean(abs(shap_value))
  shap_order <- sapply(
    X = shap_vals,
    FUN = function(x) {
      mean(abs(x))
    },
    simplify = TRUE,
    USE.NAMES = TRUE
  ) %>%
    sort(decreasing = TRUE)
  
  shap_vals$shap_id <- 1:nrow(object)
  
  # prepare original data for plotting
  num_vars <- sapply(original_data, is.numeric)
  cat_vars <- colnames(original_data)[!num_vars]
  
  # make factors to numeric
  if (length(cat_vars) > 0) {
    original_data <- original_data %>%
      dplyr::mutate_all(as.numeric)
  }
  
  # scale original data
  original_data_scaled <- original_data %>%
    scale() %>%
    tibble::as_tibble()
  # create id
  original_data_scaled$shap_id <- 1:nrow(original_data)
  
  
  # reshape data to long format for plotting
  plt_dat <- shap_vals %>%
    tidyr::pivot_longer(
      cols = setdiff(colnames(shap_vals), "shap_id"),
      names_to = "variable",
      values_to = "value"
    ) %>%
    # inner join avoids missings
    dplyr::inner_join(
      y = tidyr::pivot_longer(
        data = original_data_scaled,
        cols = setdiff(colnames(original_data_scaled), "shap_id"),
        names_to = "variable",
        values_to = "value"
      ),
      by = c("shap_id", "variable"),
      suffix = c("_shap", "_original")
    )
  
  # make variable names an ordered factor for plotting in order by mean(abs(val))
  plt_dat <- plt_dat %>%
    dplyr::mutate(variable = factor(
      x = get("variable"),
      levels = rev(names(shap_order))
    ))
   return(plt_dat)
}


#' Internal function to create the beeswarm plot
#' 
#' Creates a ggplot2-based beeswarm plot
#' 
#' @param plt_dat A tibble created with internal function
#'   \code{beeswarm_prepare}.
#' 
#' @return A ggplot2 object containing the beeswarm plot.
#'
beeswarm_plot <- function(plt_dat) {
  p <- plt_dat %>%
    ggplot2::ggplot(mapping = ggplot2::aes_string(
      x = "variable", y = "value_shap", color = "value_original"
    )) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::geom_point(position = ggplot2::position_jitter()) +
    #ggforce::geom_sina() +
    viridis::scale_color_viridis(
      option = "inferno",
      breaks = c(min(plt_dat$value_original), max(plt_dat$value_original)),
      labels = c("Low", "High")) +
    ggplot2::xlab(ggplot2::element_blank()) + 
    ggplot2::ylab("Shapley value") +
    ggplot2::theme(legend.title = ggplot2::element_text(angle = 90)) +
    ggplot2::guides(color = ggplot2::guide_colorbar(
      title = "Feature value",
      title.position = "right",
      title.hjust = 0.5
    ))
  return(p)
}
