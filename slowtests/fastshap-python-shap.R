# Load required packages
library(fastshap)
library(xgboost)

# Load the Boston housing data
boston <- pdp::boston
X <- data.matrix(subset(boston, select = -cmedv))

# Fit a quick XGBoost model
set.seed(101)  # for reproducibility
xgb <- xgboost(data = X, label = boston$cmedv, eta = 0.01, nrounds = 100,
               verbose = 0)

# Compute (approximate) SHAP values; should take about 6.5 seconds
set.seed(102)  # for reproducibility
system.time(
  shap <- fastshap(xgb, X = X, nsim = 100, pred_wrapper = pfun)
)

# Plot results
gridExtra::grid.arrange(
  autoplot(shap),
  autoplot(shap, type = "dependence", feature = "rm", X = as.data.frame(X),
           color_by = "rad") +
    scale_color_viridis_c(),
  nrow = 1
)


# # SHAP summary plot
# 
# # shap_long <- cbind("row_id" = seq_len(nrow(shap)), shap)
# 
# # shap_long <- cbind("row_id" = seq_len(nrow(shap)), shap)
# shap_long <- stats::reshape(
#   data = as.data.frame(shap),
#   direction = "long",
#   varying = names(shap),
#   v.names = "shap_value",
#   timevar = "feature",
#   times = names(shap)
# )
# 
# shap_vi <- data.frame(
#   "feature" = names(shap),
#   "importance" = apply(shap, MARGIN = 2, FUN = function(x) mean(abs(x)))
# )
# 
# shap_long <- merge(
#   x = shap_long,
#   y = shap_vi,
#   by = "feature"
# )
# 
# X_scaled <- as.data.frame(X)
# X_scaled[] <- lapply(X_scaled, FUN = function(x) {
#   scale(predict(caret::BoxCoxTrans(x), newdata = x))
#   # (x - mean(x)) / sd(x)
#   # (x - min(x)) / (max(x) - min(x)) - 0.5
#   # cut(x, breaks = 100, labels = FALSE)
# })
# X_scaled_long <- stats::reshape(
#   data = X_scaled,
#   direction = "long",
#   varying = names(X_scaled),
#   v.names = "feature_value",
#   timevar = "feature",
#   times = names(X_scaled)
# )
# 
# shap_long$feature_value <- X_scaled_long$feature_value
# 
# ggplot(shap_long, aes(x = shap_value, y = reorder(feature, importance))) +
#   geom_quasirandom(aes(color = feature_value), #method = "smiley",
#                    groupOnX = FALSE, varwidth = TRUE, size = 0.8, alpha = 0.5) +
#   scale_colour_viridis_c(name = "Feature\nvalue", option = "A") +
#   # scale_color_brewer(palette = "RdBu") +
#   # scale_color_gradient2(low = "red", mid = "white", high = "blue") +
#   xlab("SHAP value") +
#   ylab("") +
#   theme_light()
