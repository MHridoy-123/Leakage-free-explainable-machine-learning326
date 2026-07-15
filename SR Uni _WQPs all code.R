
installed <- rownames(installed.packages())
for (p in packages) {
  if (!(p %in% installed)) install.packages(p, dependencies = TRUE)
}

lapply(packages, library, character.only = TRUE)

set.seed(123)
packages <- c(
  "tidyverse", "readxl", "janitor", "skimr", "naniar",
  "GGally", "corrplot", "ggcorrplot",
  "FactoMineR", "factoextra",
  "caret", "glmnet", "ranger", "xgboost", "kernlab",
  "pdp", "iml",
  "cluster", "NbClust",
  "patchwork", "ggpubr",
  "Metrics", "yardstick",
  "Boruta"
)

installed <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed)) install.packages(p, dependencies = TRUE)
}

lapply(packages, library, character.only = TRUE)

cannot open file 'results/....csv': No such file or directory

if (!dir.exists("results")) dir.create("results")
if (!dir.exists("figures")) dir.create("figures")




library(readxl)
df_raw  <- read_excel("C:/Users/PINKI AKTER/Downloads/WQPs/SR University_WQPs datset.xlsx")
View(SR_University_WQPs_datset)



)

# Check structure
str(df_raw)
head(df_raw)
names(df_raw)

# Rename columns manually for reproducibility
names(df_raw)[1:26] <- c(
  "sample_id",
  "turbidity",
  "ec",
  "tds",
  "ph",
  "th",
  "ca",
  "mg",
  "na",
  "k",
  "fe",
  "mn",
  "nh3",
  "no2",
  "no3",
  "cl",
  "f",
  "so4",
  "po4",
  "turbidity_si",
  "ph_si",
  "th_si",
  "ca_si",
  "mg_si",
  "no3_si",
  "wqi"
)

# Convert numeric columns
df <- df_raw %>%
  mutate(across(-sample_id, ~ as.numeric(.)))

# Basic check
glimpse(df)
summary(df)






# ============================================================
# 2. Define raw predictors and target
# ============================================================

raw_predictors <- c(
  "turbidity", "ec", "tds", "ph", "th", "ca", "mg", "na",
  "k", "fe", "mn", "nh3", "no2", "no3", "cl", "f", "so4", "po4"
)

subindex_vars <- c(
  "turbidity_si", "ph_si", "th_si", "ca_si", "mg_si", "no3_si"
)

target <- "wqi"

ml_data <- df %>%
  select(sample_id, all_of(raw_predictors), wqi)

# Missing values
colSums(is.na(ml_data))

# Simple median imputation for Mn missing value
ml_data <- ml_data %>%
  mutate(across(all_of(raw_predictors), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

colSums(is.na(ml_data))





# ============================================================
# 3. Descriptive statistics
# ============================================================

desc_table <- ml_data %>%
  select(-sample_id) %>%
  summarise(across(
    everything(),
    list(
      min = ~ min(.),
      q1 = ~ quantile(., 0.25),
      median = ~ median(.),
      mean = ~ mean(.),
      q3 = ~ quantile(., 0.75),
      max = ~ max(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.) * 100
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("parameter", ".value"),
    names_pattern = "(.+)_(min|q1|median|mean|q3|max|sd|cv)"
  )

write.csv(desc_table, "results/descriptive_statistics.csv", row.names = FALSE)

print(desc_table)









# ============================================================
# 4. WQI distribution and classification
# ============================================================

summary(ml_data$wqi)
quantile(ml_data$wqi, probs = c(0, 0.25, 0.5, 0.75, 1))

ml_data <- ml_data %>%
  mutate(
    wqi_class = case_when(
      wqi <= quantile(wqi, 1/3) ~ "Low quality",
      wqi <= quantile(wqi, 2/3) ~ "Moderate quality",
      TRUE ~ "Better quality"
    ),
    wqi_class = factor(wqi_class, levels = c("Low quality", "Moderate quality", "Better quality"))
  )

table(ml_data$wqi_class)

p_wqi_hist <- ggplot(ml_data, aes(x = wqi)) +
  geom_histogram(bins = 12, color = "black", fill = "skyblue") +
  geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
  theme_bw() +
  labs(
    title = "Distribution of Water Quality Index",
    x = "Water Quality Index",
    y = "Frequency"
  )

ggsave("figures/wqi_distribution.png", p_wqi_hist, width = 7, height = 5, dpi = 300)

p_wqi_box <- ggplot(ml_data, aes(y = wqi)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  geom_jitter(width = 0.1, alpha = 0.7) +
  theme_bw() +
  labs(
    title = "Boxplot of Water Quality Index",
    y = "Water Quality Index"
  )

ggsave("figures/wqi_boxplot.png", p_wqi_box, width = 5, height = 5, dpi = 300)




# ============================================================
# 5. Parameter-wise boxplots
# ============================================================

long_raw <- ml_data %>%
  select(all_of(raw_predictors)) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "value")

p_box_all <- ggplot(long_raw, aes(x = parameter, y = value)) +
  geom_boxplot(fill = "purple", outlier.color = "red") +
  facet_wrap(~ parameter, scales = "free", ncol = 4) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  labs(
    title = "",
    x = NULL,
    y = "Concentration"
  )

ggsave("figures/all_parameter_boxplots.png", p_box_all, width = 12, height = 9, dpi = 300)




# ============================================================
# 6. Correlation analysis
# ============================================================

corr_data <- ml_data %>%
  select(all_of(raw_predictors), wqi)

corr_matrix <- cor(corr_data, method = "spearman", use = "pairwise.complete.obs")

write.csv(corr_matrix, "results/spearman_correlation_matrix.csv")

png("figures/spearman_correlation_heatmap.png", width = 2600, height = 2200, res = 300)
corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.55,
  col = colorRampPalette(c("blue", "white", "red"))(200)
)
dev.off()

# Correlation with WQI only
wqi_corr <- corr_matrix[, "wqi"] %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  rename(spearman_r = ".") %>%
  filter(parameter != "wqi") %>%
  arrange(abs(spearman_r))

p_corr_wqi <- ggplot(wqi_corr, aes(x = reorder(parameter, spearman_r), y = spearman_r)) +
  geom_col(aes(fill = spearman_r > 0)) +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = c("tomato", "steelblue"), guide = "none") +
  labs(
    title = "Spearman Correlation Between Water Quality Parameters and WQI",
    x = "Parameter",
    y = "Spearman correlation coefficient"
  )

ggsave("figures/wqi_parameter_correlation.png", p_corr_wqi, width = 8, height = 6, dpi = 300)

write.csv(wqi_corr, "results/wqi_parameter_correlation.csv", row.names = FALSE)





# ============================================================
# 7. Multicollinearity check
# ============================================================

high_corr_pairs <- findCorrelation(
  cor(ml_data %>% select(all_of(raw_predictors)), use = "pairwise.complete.obs"),
  cutoff = 0.90,
  names = TRUE,
  exact = TRUE
)

high_corr_pairs

write.csv(
  data.frame(highly_correlated_variables = high_corr_pairs),
  "results/highly_correlated_variables.csv",
  row.names = FALSE
)




# ============================================================
# 8. Principal Component Analysis
# ============================================================

pca_input <- ml_data %>%
  select(all_of(raw_predictors)) %>%
  scale()

pca_res <- PCA(as.data.frame(pca_input), graph = FALSE)

# Eigenvalues
eig <- get_eigenvalue(pca_res)
write.csv(eig, "results/pca_eigenvalues.csv", row.names = FALSE)

p_scree <- fviz_eig(
  pca_res,
  addlabels = TRUE,
  barfill = "steelblue",
  barcolor = "black",
  linecolor = "red"
) +
  theme_bw() +
  labs(title = "PCA Scree Plot")

ggsave("figures/pca_scree_plot.png", p_scree, width = 7, height = 5, dpi = 300)

# Variable contribution
p_pca_var <- fviz_pca_var(
  pca_res,
  col.var = "contrib",
  gradient.cols = c("blue", "orange", "red"),
  repel = TRUE
) +
  theme_bw() +
  labs(title = "PCA Variable Contribution Plot")

ggsave("figures/pca_variable_contribution.png", p_pca_var, width = 8, height = 7, dpi = 300)

# Individual samples colored by WQI class
p_pca_ind <- fviz_pca_ind(
  pca_res,
  geom.ind = "point",
  col.ind = ml_data$wqi_class,
  palette = c("red", "orange", "darkgreen"),
  addEllipses = TRUE,
  legend.title = "WQI class"
) +
  theme_bw() +
  labs(title = "PCA Score Plot by WQI Class")

ggsave("figures/pca_samples_by_wqi_class.png", p_pca_ind, width = 8, height = 6, dpi = 300)

# PCA loadings
pca_loadings <- as.data.frame(pca_res$var$coord)
pca_loadings$parameter <- rownames(pca_loadings)
write.csv(pca_loadings, "results/pca_loadings.csv", row.names = FALSE)



















# ============================================================
# 9. Cluster analysis
# ============================================================

cluster_input <- ml_data %>%
  select(all_of(raw_predictors)) %>%
  scale()

# Determine optimal number of clusters using silhouette method
p_sil <- fviz_nbclust(
  cluster_input,
  kmeans,
  method = "silhouette"
) +
  theme_bw() +
  labs(title = "Optimal Number of Clusters: Silhouette Method")

ggsave("figures/optimal_cluster_silhouette.png", p_sil, width = 7, height = 5, dpi = 300)

# K-means with 3 clusters
set.seed(123)
km3 <- kmeans(cluster_input, centers = 3, nstart = 50)

ml_data$cluster <- factor(km3$cluster)

# Cluster vs WQI
cluster_summary <- ml_data %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    mean_wqi = mean(wqi),
    median_wqi = median(wqi),
    min_wqi = min(wqi),
    max_wqi = max(wqi),
    .groups = "drop"
  )

write.csv(cluster_summary, "results/cluster_summary.csv", row.names = FALSE)

p_cluster <- fviz_cluster(
  km3,
  data = cluster_input,
  geom = "point",
  ellipse.type = "convex"
) +
  theme_bw() +
  labs(title = "K-means Clustering of Freshwater Samples")

ggsave("figures/kmeans_cluster_plot.png", p_cluster, width = 8, height = 6, dpi = 300)

p_cluster_wqi <- ggplot(ml_data, aes(x = cluster, y = wqi, fill = cluster)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.7) +
  theme_bw() +
  labs(
    title = "WQI Distribution Across Hydrochemical Clusters",
    x = "Cluster",
    y = "Water Quality Index"
  )

ggsave("figures/cluster_wqi_boxplot.png", p_cluster_wqi, width = 7, height = 5, dpi = 300)








# ============================================================
# 10. Machine learning regression setup
# ============================================================

model_data <- ml_data %>%
  select(all_of(raw_predictors), wqi)

# Train control
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 20,
  savePredictions = "final",
  verboseIter = FALSE
)

# Preprocessing
preprocess_steps <- c("center", "scale")

set.seed(123)

# 10.1 Linear Regression
lm_model <- train(
  wqi ~ .,
  data = model_data,
  method = "lm",
  trControl = ctrl,
  preProcess = preprocess_steps,
  metric = "RMSE"
)

# 10.2 Ridge Regression
ridge_grid <- expand.grid(
  alpha = 0,
  lambda = 10^seq(-4, 2, length = 50)
)

ridge_model <- train(
  wqi ~ .,
  data = model_data,
  method = "glmnet",
  trControl = ctrl,
  preProcess = preprocess_steps,
  tuneGrid = ridge_grid,
  metric = "RMSE"
)

# 10.3 Lasso Regression
lasso_grid <- expand.grid(
  alpha = 1,
  lambda = 10^seq(-4, 2, length = 50)
)

lasso_model <- train(
  wqi ~ .,
  data = model_data,
  method = "glmnet",
  trControl = ctrl,
  preProcess = preprocess_steps,
  tuneGrid = lasso_grid,
  metric = "RMSE"
)

# 10.4 Elastic Net
enet_grid <- expand.grid(
  alpha = seq(0.1, 0.9, by = 0.2),
  lambda = 10^seq(-4, 2, length = 30)
)

enet_model <- train(
  wqi ~ .,
  data = model_data,
  method = "glmnet",
  trControl = ctrl,
  preProcess = preprocess_steps,
  tuneGrid = enet_grid,
  metric = "RMSE"
)

# 10.5 Random Forest
rf_grid <- expand.grid(
  mtry = c(3, 5, 7, 9, 12),
  splitrule = "variance",
  min.node.size = c(3, 5, 7)
)

rf_model <- train(
  wqi ~ .,
  data = model_data,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = rf_grid,
  importance = "permutation",
  metric = "RMSE"
)

# 10.6 Support Vector Regression
svr_model <- train(
  wqi ~ .,
  data = model_data,
  method = "svmRadial",
  trControl = ctrl,
  preProcess = preprocess_steps,
  tuneLength = 10,
  metric = "RMSE"
)

# 10.7 XGBoost
xgb_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(2, 3, 4),
  eta = c(0.01, 0.05, 0.1),
  gamma = 0,
  colsample_bytree = c(0.7, 0.9),
  min_child_weight = c(1, 3),
  subsample = c(0.7, 0.9)
)

xgb_model <- train(
  wqi ~ .,
  data = model_data,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = xgb_grid,
  metric = "RMSE",
  verbose = FALSE
)

  

ls()


exists("model_data")
exists("ctrl")
exists("xgb_grid")



# ============================================================
# ML-ready data
# ============================================================

raw_predictors <- c(
  "turbidity", "ec", "tds", "ph", "th", "ca", "mg", "na",
  "k", "fe", "mn", "nh3", "no2", "no3", "cl", "f", "so4", "po4"
)

model_data <- ml_data %>%
  select(all_of(raw_predictors), wqi)

# Check missing value
colSums(is.na(model_data))

# If any missing remains, median imputation
model_data <- model_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

str(model_data)



library(caret)
library(glmnet)
library(ranger)
library(xgboost)
library(kernlab)
library(Metrics)
library(tidyverse)

set.seed(123)





ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  savePredictions = "final",
  verboseIter = FALSE,
  allowParallel = FALSE
)


set.seed(123)















xgb_grid <- expand.grid(
  nrounds = c(50, 100),
  max_depth = c(2, 3),
  eta = c(0.03, 0.1),
  gamma = 0,
  colsample_bytree = c(0.7, 0.9),
  min_child_weight = c(1, 3),
  subsample = c(0.7, 0.9)
)

xgb_grid



set.seed(123)

xgb_model <- train(
  wqi ~ .,
  data = model_data,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = xgb_grid,
  metric = "RMSE",
  verbose = FALSE
)

xgb_model



warnings()











# ============================================================
# Direct XGBoost repeated cross-validation
# ============================================================

library(xgboost)
library(caret)
library(dplyr)

set.seed(123)

# Input matrix and target
x <- model_data %>%
  select(-wqi) %>%
  as.matrix()

y <- model_data$wqi

# Check
dim(x)
length(y)

# Repeated CV setup
folds <- createMultiFolds(y, k = 10, times = 10)

# XGBoost parameter grid: small dataset-er jonno conservative grid
xgb_params_grid <- expand.grid(
  eta = c(0.03, 0.05, 0.10),
  max_depth = c(2, 3),
  subsample = c(0.70, 0.90),
  colsample_bytree = c(0.70, 0.90),
  min_child_weight = c(1, 3),
  nrounds = c(50, 100)
)

xgb_cv_results <- data.frame()

for (i in 1:nrow(xgb_params_grid)) {
  
  params_now <- xgb_params_grid[i, ]
  
  pred_all <- rep(NA, length(y))
  obs_all <- y
  
  for (fold_name in names(folds)) {
    
    train_index <- folds[[fold_name]]
    test_index <- setdiff(seq_along(y), train_index)
    
    dtrain <- xgb.DMatrix(data = x[train_index, ], label = y[train_index])
    dtest <- xgb.DMatrix(data = x[test_index, ], label = y[test_index])
    
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = params_now$eta,
      max_depth = params_now$max_depth,
      subsample = params_now$subsample,
      colsample_bytree = params_now$colsample_bytree,
      min_child_weight = params_now$min_child_weight
    )
    
    fit <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = params_now$nrounds,
      verbose = 0
    )
    
    pred_all[test_index] <- predict(fit, dtest)
  }
  
  rmse_now <- sqrt(mean((obs_all - pred_all)^2, na.rm = TRUE))
  mae_now <- mean(abs(obs_all - pred_all), na.rm = TRUE)
  r2_now <- cor(obs_all, pred_all, use = "complete.obs")^2
  
  xgb_cv_results <- rbind(
    xgb_cv_results,
    data.frame(
      model = "XGBoost_direct",
      eta = params_now$eta,
      max_depth = params_now$max_depth,
      subsample = params_now$subsample,
      colsample_bytree = params_now$colsample_bytree,
      min_child_weight = params_now$min_child_weight,
      nrounds = params_now$nrounds,
      RMSE = rmse_now,
      MAE = mae_now,
      R2 = r2_now
    )
  )
}

xgb_cv_results <- xgb_cv_results %>%
  arrange(RMSE)

xgb_cv_results

write.csv(xgb_cv_results, "results/xgboost_direct_cv_results.csv", row.names = FALSE)

best_xgb <- xgb_cv_results[1, ]
best_xgb









# ============================================================
# Final XGBoost model using best parameters
# ============================================================

dtrain_full <- xgb.DMatrix(data = x, label = y)

best_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = best_xgb$eta,
  max_depth = best_xgb$max_depth,
  subsample = best_xgb$subsample,
  colsample_bytree = best_xgb$colsample_bytree,
  min_child_weight = best_xgb$min_child_weight
)

final_xgb_model <- xgb.train(
  params = best_params,
  data = dtrain_full,
  nrounds = best_xgb$nrounds,
  verbose = 0
)

final_xgb_model




rm(list = c(
  "xgb_cv_results", "best_xgb", "final_xgb_model",
  "xgb_params_grid"
))




# ============================================================
# Fixed direct XGBoost repeated cross-validation
# ============================================================

library(xgboost)
library(caret)
library(dplyr)
library(ggplot2)

set.seed(123)

# Input matrix and target
x <- model_data %>%
  select(-wqi) %>%
  as.data.frame() %>%
  as.matrix()

y <- as.numeric(model_data$wqi)

# Check dimensions
dim(x)
length(y)

# Create repeated folds
folds <- createMultiFolds(y, k = 10, times = 10)

# Conservative grid for small dataset
xgb_params_grid <- expand.grid(
  eta = c(0.03, 0.05, 0.10),
  max_depth = c(2, 3),
  subsample = c(0.70, 0.90),
  colsample_bytree = c(0.70, 0.90),
  min_child_weight = c(1, 3),
  nrounds = c(50, 100),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

xgb_cv_results <- data.frame()

for (i in seq_len(nrow(xgb_params_grid))) {
  
  params_now <- xgb_params_grid[i, ]
  
  eta_now <- as.numeric(params_now[["eta"]])
  max_depth_now <- as.integer(params_now[["max_depth"]])
  subsample_now <- as.numeric(params_now[["subsample"]])
  colsample_now <- as.numeric(params_now[["colsample_bytree"]])
  min_child_now <- as.numeric(params_now[["min_child_weight"]])
  nrounds_now <- as.integer(params_now[["nrounds"]])
  
  fold_results <- data.frame()
  
  for (fold_name in names(folds)) {
    
    train_index <- folds[[fold_name]]
    test_index <- setdiff(seq_along(y), train_index)
    
    dtrain <- xgb.DMatrix(data = x[train_index, , drop = FALSE], label = y[train_index])
    dtest <- xgb.DMatrix(data = x[test_index, , drop = FALSE], label = y[test_index])
    
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = eta_now,
      max_depth = max_depth_now,
      subsample = subsample_now,
      colsample_bytree = colsample_now,
      min_child_weight = min_child_now
    )
    
    fit <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = nrounds_now,
      verbose = 0
    )
    
    pred <- predict(fit, dtest)
    
    fold_results <- rbind(
      fold_results,
      data.frame(
        fold = fold_name,
        obs = y[test_index],
        pred = pred
      )
    )
  }
  
  rmse_now <- sqrt(mean((fold_results$obs - fold_results$pred)^2))
  mae_now <- mean(abs(fold_results$obs - fold_results$pred))
  r2_now <- cor(fold_results$obs, fold_results$pred)^2
  
  xgb_cv_results <- rbind(
    xgb_cv_results,
    data.frame(
      model = "XGBoost_direct",
      eta = eta_now,
      max_depth = max_depth_now,
      subsample = subsample_now,
      colsample_bytree = colsample_now,
      min_child_weight = min_child_now,
      nrounds = nrounds_now,
      RMSE = rmse_now,
      MAE = mae_now,
      R2 = r2_now
    )
  )
}

xgb_cv_results <- xgb_cv_results %>%
  arrange(RMSE)

xgb_cv_results

if (!dir.exists("results")) dir.create("results")
write.csv(xgb_cv_results, "results/xgboost_direct_cv_results.csv", row.names = FALSE)


head(xgb_cv_results)
names(xgb_cv_results)
nrow(xgb_cv_results)




best_xgb <- xgb_cv_results[1, ]

best_xgb
best_xgb$nrounds








# ============================================================
# Final XGBoost model using best CV parameters
# ============================================================

dtrain_full <- xgb.DMatrix(data = x, label = y)

best_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = as.numeric(best_xgb[["eta"]]),
  max_depth = as.integer(best_xgb[["max_depth"]]),
  subsample = as.numeric(best_xgb[["subsample"]]),
  colsample_bytree = as.numeric(best_xgb[["colsample_bytree"]]),
  min_child_weight = as.numeric(best_xgb[["min_child_weight"]])
)

final_xgb_model <- xgb.train(
  params = best_params,
  data = dtrain_full,
  nrounds = as.integer(best_xgb[["nrounds"]]),
  verbose = 0
)

final_xgb_model








xgb_importance <- xgb.importance(
  feature_names = colnames(x),
  model = final_xgb_model
)

xgb_importance

write.csv(xgb_importance, "results/xgboost_variable_importance.csv", row.names = FALSE)

if (!dir.exists("figures")) dir.create("figures")

png("figures/xgboost_variable_importance.png", width = 2000, height = 1600, res = 300)
xgb.plot.importance(xgb_importance, top_n = 15)
dev.off()












names(best_xgb)
best_xgb
best_xgb[["nrounds"]]
length(best_xgb[["nrounds"]])


# ============================================================
# Safe extraction of best XGBoost parameters
# ============================================================

best_xgb <- xgb_cv_results[1, , drop = FALSE]

print(best_xgb)
print(names(best_xgb))

# Safe nrounds extraction
if ("nrounds" %in% names(best_xgb)) {
  nrounds_final <- as.integer(best_xgb$nrounds[1])
} else {
  nrounds_final <- 100L
}

nrounds_final
length(nrounds_final)




# ============================================================
# Final XGBoost model - safe version
# ============================================================

dtrain_full <- xgb.DMatrix(data = x, label = y)

best_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = as.numeric(best_xgb$eta[1]),
  max_depth = as.integer(best_xgb$max_depth[1]),
  subsample = as.numeric(best_xgb$subsample[1]),
  colsample_bytree = as.numeric(best_xgb$colsample_bytree[1]),
  min_child_weight = as.numeric(best_xgb$min_child_weight[1])
)

final_xgb_model <- xgb.train(
  params = best_params,
  data = dtrain_full,
  nrounds = nrounds_final,
  verbose = 0
)

final_xgb_model





# ============================================================
# Manual final XGBoost model
# ============================================================

dtrain_full <- xgb.DMatrix(data = x, label = y)

manual_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 2,
  subsample = 0.90,
  colsample_bytree = 0.90,
  min_child_weight = 3
)

final_xgb_model <- xgb.train(
  params = manual_params,
  data = dtrain_full,
  nrounds = 100L,
  verbose = 0
)

final_xgb_model


xgb_importance <- xgb.importance(
  feature_names = colnames(x),
  model = final_xgb_model
)

xgb_importance

if (!dir.exists("results")) dir.create("results")
if (!dir.exists("figures")) dir.create("figures")

write.csv(xgb_importance, "results/xgboost_variable_importance.csv", row.names = FALSE)

png("figures/xgboost_variable_importance.png", width = 2000, height = 1600, res = 300)
xgb.plot.importance(xgb_importance, top_n = 15)
dev.off()





ls()


exists("lm_model")
exists("ridge_model")
exists("lasso_model")
exists("enet_model")
exists("rf_model")
exists("svr_model")




# ============================================================
# Fix model_perf: create model list from available caret models
# ============================================================

models <- list()

if (exists("lm_model")) {
  models$Linear_Model <- lm_model
}

if (exists("ridge_model")) {
  models$Ridge <- ridge_model
}

if (exists("lasso_model")) {
  models$Lasso <- lasso_model
}

if (exists("enet_model")) {
  models$Elastic_Net <- enet_model
}

if (exists("rf_model")) {
  models$Random_Forest <- rf_model
}

if (exists("svr_model")) {
  models$SVR <- svr_model
}

names(models)
length(models)



if (!dir.exists("results")) dir.create("results")

write.csv(model_perf, "results/model_performance_rmse.csv", row.names = FALSE)



if (!dir.exists("figures")) dir.create("figures")

png("figures/model_rmse_comparison.png", width = 2200, height = 1600, res = 300)
bwplot(resamples_all, metric = "RMSE", main = "Model Comparison Based on Cross-validated RMSE")
dev.off()




while (!is.null(dev.list())) dev.off()



# ============================================================
# FIX: Create model_perf without caret::resamples()
# ============================================================

library(tidyverse)
library(caret)

if (!dir.exists("results")) dir.create("results")
if (!dir.exists("figures")) dir.create("figures")

# Build available model list
models <- list()

if (exists("lm_model")) models$Linear_Model <- lm_model
if (exists("ridge_model")) models$Ridge <- ridge_model
if (exists("lasso_model")) models$Lasso <- lasso_model
if (exists("enet_model")) models$Elastic_Net <- enet_model
if (exists("rf_model")) models$Random_Forest <- rf_model
if (exists("svr_model")) models$SVR <- svr_model

names(models)
length(models)









# ============================================================
# Extract best RMSE, MAE, R2 from each caret model
# ============================================================

get_best_perf <- function(model_object, model_name) {
  
  res <- model_object$results
  
  # Best row based on lowest RMSE
  best_row <- res[which.min(res$RMSE), , drop = FALSE]
  
  data.frame(
    model = model_name,
    RMSE_mean = best_row$RMSE,
    RMSE_sd = ifelse("RMSESD" %in% names(best_row), best_row$RMSESD, NA),
    MAE_mean = ifelse("MAE" %in% names(best_row), best_row$MAE, NA),
    MAE_sd = ifelse("MAESD" %in% names(best_row), best_row$MAESD, NA),
    R2_mean = ifelse("Rsquared" %in% names(best_row), best_row$Rsquared, NA),
    R2_sd = ifelse("RsquaredSD" %in% names(best_row), best_row$RsquaredSD, NA)
  )
}

model_perf <- bind_rows(
  Map(get_best_perf, models, names(models))
) %>%
  arrange(RMSE_mean)

model_perf

write.csv(model_perf, "results/model_performance_rmse.csv", row.names = FALSE)








# ============================================================
# Add native XGBoost performance
# ============================================================

if (exists("xgb_cv_results")) {
  
  best_xgb <- xgb_cv_results[1, , drop = FALSE]
  
  xgb_perf <- data.frame(
    model = "XGBoost_direct",
    RMSE_mean = best_xgb$RMSE[1],
    RMSE_sd = NA,
    MAE_mean = best_xgb$MAE[1],
    MAE_sd = NA,
    R2_mean = best_xgb$R2[1],
    R2_sd = NA
  )
  
  combined_model_perf <- bind_rows(model_perf, xgb_perf) %>%
    arrange(RMSE_mean)
  
} else {
  
  combined_model_perf <- model_perf
  
}

combined_model_perf

write.csv(combined_model_perf, "results/combined_model_performance.csv", row.names = FALSE)












exists("xgb_cv_results")
names(xgb_cv_results)
head(xgb_cv_results)
str(xgb_cv_results)









# ============================================================
# FIX XGBoost result column names
# ============================================================

library(dplyr)
library(tibble)

# Check object
if (!exists("xgb_cv_results")) {
  stop("xgb_cv_results object not found. Please run direct XGBoost CV code first.")
}

# Convert column names to lowercase for safety
names(xgb_cv_results) <- tolower(names(xgb_cv_results))

names(xgb_cv_results)
head(xgb_cv_results)












# ============================================================
# FIX XGBoost result column names
# ============================================================

library(dplyr)
library(tibble)

# Check object
if (!exists("xgb_cv_results")) {
  stop("xgb_cv_results object not found. Please run direct XGBoost CV code first.")
}

# Convert column names to lowercase for safety
names(xgb_cv_results) <- tolower(names(xgb_cv_results))

names(xgb_cv_results)
head(xgb_cv_results)







"rmse" %in% names(xgb_cv_results)
"mae" %in% names(xgb_cv_results)
"r2" %in% names(xgb_cv_results)








TRUE
TRUE
TRUE









# ============================================================
# Select best XGBoost result safely
# ============================================================

best_xgb <- xgb_cv_results %>%
  arrange(rmse) %>%
  slice(1)

best_xgb

best_xgb$rmse[1]
best_xgb$mae[1]
best_xgb$r2[1]








# ============================================================
# Combine caret model performance with fixed XGBoost result
# ============================================================

xgb_perf <- data.frame(
  model = "XGBoost_direct",
  RMSE_mean = best_xgb$rmse[1],
  RMSE_sd = NA,
  MAE_mean = best_xgb$mae[1],
  MAE_sd = NA,
  R2_mean = best_xgb$r2[1],
  R2_sd = NA
)

combined_model_perf <- bind_rows(model_perf, xgb_perf) %>%
  arrange(RMSE_mean)

combined_model_perf

write.csv(combined_model_perf, "results/combined_model_performance.csv", row.names = FALSE)











# ============================================================
# Robust fix for XGBoost performance extraction
# ============================================================

library(dplyr)
library(tibble)
library(ggplot2)

if (!dir.exists("results")) dir.create("results")
if (!dir.exists("figures")) dir.create("figures")

# Check object
if (!exists("xgb_cv_results")) {
  stop("xgb_cv_results object not found. Please run XGBoost CV first.")
}

# Print names for checking
print(names(xgb_cv_results))
print(head(xgb_cv_results))

# Find columns safely, case-insensitive
nm <- names(xgb_cv_results)
nm_low <- tolower(nm)

rmse_col <- nm[which(nm_low %in% c("rmse", "rmse_mean", "xgb_rmse"))[1]]
mae_col  <- nm[which(nm_low %in% c("mae", "mae_mean", "xgb_mae"))[1]]
r2_col   <- nm[which(nm_low %in% c("r2", "rsquared", "r_squared", "r2_mean"))[1]]

print(rmse_col)
print(mae_col)
print(r2_col)

if (is.na(rmse_col)) {
  stop("No RMSE column found in xgb_cv_results. Please send names(xgb_cv_results) output.")
}

# Select best XGBoost row safely
best_xgb <- xgb_cv_results %>%
  arrange(.data[[rmse_col]]) %>%
  slice(1)

best_xgb
``




if (exists("xgb_cv_results")) rm(xgb_cv_results)
if (exists("best_xgb")) rm(best_xgb)
if (exists("xgb_perf")) rm(xgb_perf)










# ============================================================
# CLEAN XGBoost CV table - conflict-free
# ============================================================

library(xgboost)
library(caret)
library(dplyr)

set.seed(123)

# Prepare X and y
x <- model_data %>%
  select(-wqi) %>%
  as.data.frame() %>%
  as.matrix()

y <- as.numeric(model_data$wqi)

# Repeated folds
folds <- createMultiFolds(y, k = 10, times = 10)

# Small, conservative parameter grid
xgb_grid_clean <- expand.grid(
  eta_value = c(0.03, 0.05, 0.10),
  depth_value = c(2, 3),
  subsample_value = c(0.70, 0.90),
  colsample_value = c(0.70, 0.90),
  child_weight_value = c(1, 3),
  rounds_value = c(50, 100),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

xgb_cv_table <- data.frame()

for (i in seq_len(nrow(xgb_grid_clean))) {
  
  g <- xgb_grid_clean[i, ]
  
  all_obs <- c()
  all_pred <- c()
  
  for (fold_name in names(folds)) {
    
    train_index <- folds[[fold_name]]
    test_index <- setdiff(seq_along(y), train_index)
    
    dtrain <- xgb.DMatrix(
      data = x[train_index, , drop = FALSE],
      label = y[train_index]
    )
    
    dtest <- xgb.DMatrix(
      data = x[test_index, , drop = FALSE],
      label = y[test_index]
    )
    
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = as.numeric(g$eta_value),
      max_depth = as.integer(g$depth_value),
      subsample = as.numeric(g$subsample_value),
      colsample_bytree = as.numeric(g$colsample_value),
      min_child_weight = as.numeric(g$child_weight_value)
    )
    
    fit <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = as.integer(g$rounds_value),
      verbose = 0
    )
    
    pred <- predict(fit, dtest)
    
    all_obs <- c(all_obs, y[test_index])
    all_pred <- c(all_pred, pred)
  }
  
  xgb_RMSE_value <- sqrt(mean((all_obs - all_pred)^2))
  xgb_MAE_value <- mean(abs(all_obs - all_pred))
  xgb_R2_value <- cor(all_obs, all_pred)^2
  
  xgb_cv_table <- rbind(
    xgb_cv_table,
    data.frame(
      model = "XGBoost_direct",
      eta_value = as.numeric(g$eta_value),
      depth_value = as.integer(g$depth_value),
      subsample_value = as.numeric(g$subsample_value),
      colsample_value = as.numeric(g$colsample_value),
      child_weight_value = as.numeric(g$child_weight_value),
      rounds_value = as.integer(g$rounds_value),
      xgb_RMSE_value = xgb_RMSE_value,
      xgb_MAE_value = xgb_MAE_value,
      xgb_R2_value = xgb_R2_value
    )
  )
}

xgb_cv_table <- xgb_cv_table %>%
  arrange(xgb_RMSE_value)

xgb_cv_table

if (!dir.exists("results")) dir.create("results")
write.csv(xgb_cv_table, "results/xgboost_clean_cv_results.csv", row.names = FALSE)





names(xgb_cv_table)
head(xgb_cv_table)




"xgb_RMSE_value"
"xgb_MAE_value"
"xgb_R2_value"


best_xgb <- xgb_cv_table[1, , drop = FALSE]

best_xgb


xgb_perf <- data.frame(
  model = "XGBoost_direct",
  RMSE_mean = best_xgb$xgb_RMSE_value[1],
  RMSE_sd = NA,
  MAE_mean = best_xgb$xgb_MAE_value[1],
  MAE_sd = NA,
  R2_mean = best_xgb$xgb_R2_value[1],
  R2_sd = NA
)

xgb_perf



combined_model_perf <- bind_rows(model_perf, xgb_perf) %>%
  arrange(RMSE_mean)

combined_model_perf

write.csv(combined_model_perf, "results/combined_model_performance.csv", row.names = FALSE)







library(ggplot2)

p_combined_perf <- ggplot(combined_model_perf, aes(x = reorder(model, RMSE_mean), y = RMSE_mean)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Comparison of Machine Learning Models for WQI Prediction",
    x = "Model",
    y = "Cross-validated RMSE"
  )

p_combined_perf

if (!dir.exists("figures")) dir.create("figures")

ggsave("figures/combined_model_performance_rmse.png", p_combined_perf, width = 7, height = 5, dpi = 300)










# ============================================================
# 1. Identify best model from combined performance table
# ============================================================

best_model_overall <- combined_model_perf$model[1]
best_model_overall

write.csv(
  data.frame(best_model_overall = best_model_overall),
  "results/best_model_overall.csv",
  row.names = FALSE
)













# ============================================================
# 2. Random Forest variable importance
# ============================================================

library(caret)
library(tidyverse)

rf_importance <- varImp(rf_model)

rf_vip_df <- rf_importance$importance %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  arrange(desc(Overall))

rf_vip_df

write.csv(rf_vip_df, "results/random_forest_variable_importance.csv", row.names = FALSE)

p_rf_vip <- ggplot(rf_vip_df, aes(x = reorder(parameter, Overall), y = Overall)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Random Forest Variable Importance for WQI Prediction",
    x = "Parameter",
    y = "Importance"
  )

p_rf_vip

ggsave("figures/random_forest_variable_importance.png", p_rf_vip, width = 7, height = 6, dpi = 300)




# ============================================================
# 3. XGBoost variable importance
# ============================================================

library(xgboost)

xgb_importance <- xgb.importance(
  feature_names = colnames(x),
  model = final_xgb_model
)

xgb_importance

write.csv(xgb_importance, "results/xgboost_variable_importance.csv", row.names = FALSE)

png("figures/xgboost_variable_importance.png", width = 2200, height = 1700, res = 300)
xgb.plot.importance(xgb_importance, top_n = 15)
dev.off()




# ============================================================
# 4. Partial dependence plots using Random Forest
# ============================================================

library(pdp)
library(ggplot2)

top6_rf_vars <- rf_vip_df %>%
  slice(1:6) %>%
  pull(parameter)

top6_rf_vars

for (v in top6_rf_vars) {
  
  pd <- partial(
    object = rf_model,
    pred.var = v,
    train = model_data,
    grid.resolution = 20
  )
  
  p_pdp <- autoplot(pd, contour = FALSE) +
    theme_bw() +
    labs(
      title = paste("Partial Dependence of WQI on", v),
      x = v,
      y = "Predicted WQI"
    )
  
  print(p_pdp)
  
  ggsave(
    paste0("figures/pdp_", v, ".png"),
    p_pdp,
    width = 6,
    height = 5,
    dpi = 300
  )
}





# ============================================================
# 5. Boruta feature selection
# ============================================================

library(Boruta)
library(tidyverse)

set.seed(123)

boruta_res <- Boruta(
  wqi ~ .,
  data = model_data,
  doTrace = 2,
  maxRuns = 500
)

boruta_fixed <- TentativeRoughFix(boruta_res)

boruta_df <- attStats(boruta_fixed) %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  arrange(desc(meanImp))

boruta_df

write.csv(boruta_df, "results/boruta_feature_selection.csv", row.names = FALSE)

png("figures/boruta_feature_selection.png", width = 2600, height = 1700, res = 300)
plot(
  boruta_fixed,
  las = 2,
  cex.axis = 0.8,
  main = "Boruta Feature Selection for WQI Prediction"
)
dev.off()

confirmed_vars <- boruta_df %>%
  filter(decision == "Confirmed") %>%
  pull(parameter)

confirmed_vars

write.csv(
  data.frame(confirmed_variables = confirmed_vars),
  "results/boruta_confirmed_variables.csv",
  row.names = FALSE
)



# ============================================================
# 6. Reduced monitoring parameter optimization
# ============================================================

library(caret)
library(ranger)
library(tidyverse)

ranked_vars <- rf_vip_df %>%
  arrange(desc(Overall)) %>%
  pull(parameter)

ranked_vars

reduced_results <- data.frame()

for (k in 2:length(ranked_vars)) {
  
  selected_vars <- ranked_vars[1:k]
  
  temp_data <- model_data %>%
    select(all_of(selected_vars), wqi)
  
  set.seed(123)
  
  temp_model <- train(
    wqi ~ .,
    data = temp_data,
    method = "ranger",
    trControl = ctrl,
    tuneLength = 5,
    importance = "permutation",
    metric = "RMSE"
  )
  
  best_row <- temp_model$results[which.min(temp_model$results$RMSE), ]
  
  reduced_results <- rbind(
    reduced_results,
    data.frame(
      number_of_parameters = k,
      selected_parameters = paste(selected_vars, collapse = ", "),
      RMSE = best_row$RMSE,
      MAE = if ("MAE" %in% names(best_row)) best_row$MAE else NA,
      R2 = best_row$Rsquared
    )
  )
}

reduced_results <- reduced_results %>%
  arrange(number_of_parameters)

reduced_results

write.csv(reduced_results, "results/reduced_parameter_optimization.csv", row.names = FALSE)



p_reduced_rmse <- ggplot(reduced_results, aes(x = number_of_parameters, y = RMSE)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(size = 2, color = "black") +
  theme_classic() +
  labs(
    title = "Monitoring Optimization: RMSE vs Number of Parameters",
    x = "Number of monitored parameters",
    y = "Cross-validated RMSE"
  )

p_reduced_rmse

ggsave("figures/reduced_parameter_rmse.png", p_reduced_rmse, width = 7, height = 5, dpi = 300)






best_rmse <- min(reduced_results$RMSE)

optimized_set <- reduced_results %>%
  filter(RMSE <= best_rmse * 1.05) %>%
  arrange(number_of_parameters) %>%
  slice(1)

optimized_set

write.csv(optimized_set, "results/optimized_monitoring_parameter_set.csv", row.names = FALSE)




# ============================================================
# FIXED Classification model for WQI class
# ============================================================

library(caret)
library(ranger)
library(tidyverse)

if (!dir.exists("results")) dir.create("results")
if (!dir.exists("figures")) dir.create("figures")

# Recreate valid WQI class names
ml_data <- ml_data %>%
  mutate(
    wqi_class = case_when(
      wqi <= quantile(wqi, 1/3) ~ "Low_quality",
      wqi <= quantile(wqi, 2/3) ~ "Moderate_quality",
      TRUE ~ "Better_quality"
    ),
    wqi_class = factor(
      wqi_class,
      levels = c("Low_quality", "Moderate_quality", "Better_quality")
    )
  )

table(ml_data$wqi_class)

# Classification dataset
class_data <- ml_data %>%
  select(all_of(raw_predictors), wqi_class)

# Check class names
levels(class_data$wqi_class)

# Cross-validation setting
class_ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  classProbs = TRUE,
  savePredictions = "final",
  allowParallel = FALSE
)

set.seed(123)

rf_class_model <- train(
  wqi_class ~ .,
  data = class_data,
  method = "ranger",
  trControl = class_ctrl,
  tuneLength = 10,
  importance = "permutation",
  metric = "Accuracy"
)

rf_class_model




## Clean Code New 


raw_predictors <- c(
  "turbidity", "ec", "tds", "ph", "th", "ca", "mg", "na",
  "k", "fe", "mn", "nh3", "no2", "no3", "cl", "f", "so4", "po4"
)

subindex_vars <- c(
  "turbidity_si", "ph_si", "th_si", "ca_si", "mg_si", "no3_si"
)

ml_data <- df %>%
  select(sample_id, all_of(raw_predictors), wqi)

colSums(is.na(ml_data))

ml_data <- ml_data %>%
  mutate(across(all_of(raw_predictors), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

colSums(is.na(ml_data))

write.csv(ml_data, "results/ml_ready_dataset.csv", row.names = FALSE)



desc_table <- ml_data %>%
  select(-sample_id) %>%
  summarise(across(
    everything(),
    list(
      min = ~ min(.),
      q1 = ~ quantile(., 0.25),
      median = ~ median(.),
      mean = ~ mean(.),
      q3 = ~ quantile(., 0.75),
      max = ~ max(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.) * 100
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("parameter", ".value"),
    names_pattern = "(.+)_(min|q1|median|mean|q3|max|sd|cv)"
  )

desc_table

write.csv(desc_table, "results/descriptive_statistics.csv", row.names = FALSE)





summary(ml_data$wqi)
quantile(ml_data$wqi, probs = c(0, 0.25, 0.5, 0.75, 1))

ml_data <- ml_data %>%
  mutate(
    wqi_class = case_when(
      wqi <= quantile(wqi, 1/3) ~ "Low_quality",
      wqi <= quantile(wqi, 2/3) ~ "Moderate_quality",
      TRUE ~ "Better_quality"
    ),
    wqi_class = factor(
      wqi_class,
      levels = c("Low_quality", "Moderate_quality", "Better_quality")
    )
  )

table(ml_data$wqi_class)

p_wqi_hist <- ggplot(ml_data, aes(x = wqi)) +
  geom_histogram(bins = 12, color = "black", fill = "skyblue") +
  geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
  theme_bw() +
  labs(
    title = "Distribution of Water Quality Index",
    x = "Water Quality Index",
    y = "Frequency"
  )

p_wqi_hist

ggsave("figures/wqi_distribution.png", p_wqi_hist, width = 7, height = 5, dpi = 300)

p_wqi_box <- ggplot(ml_data, aes(x = "", y = wqi)) +
  geom_boxplot(fill = "lightgreen", color = "black", width = 0.35) +
  geom_jitter(width = 0.08, alpha = 0.7, color = "black") +
  theme_classic() +
  labs(
    title = "Boxplot of Water Quality Index",
    x = NULL,
    y = "Water Quality Index"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

p_wqi_box

ggsave("figures/wqi_boxplot.png", p_wqi_box, width = 5, height = 5, dpi = 300)






long_raw <- ml_data %>%
  select(all_of(raw_predictors)) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "value")

p_box_all <- ggplot(long_raw, aes(x = parameter, y = value)) +
  geom_boxplot(fill = "purple", outlier.color = "red") +
  facet_wrap(~ parameter, scales = "free", ncol = 4) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  labs(
    title = "",
    x = NULL,
    y = "Concentration"
  )

p_box_all

ggsave("figures/all_parameter_boxplots.png", p_box_all, width = 12, height = 9, dpi = 300)



corr_data <- ml_data %>%
  select(all_of(raw_predictors), wqi)

corr_matrix <- cor(corr_data, method = "spearman", use = "pairwise.complete.obs")

write.csv(corr_matrix, "results/spearman_correlation_matrix.csv")

png("figures/spearman_correlation_heatmap.png", width = 2600, height = 2200, res = 300)
corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.55,
  col = colorRampPalette(c("blue", "white", "red"))(200)
)
dev.off()

wqi_corr <- corr_matrix[, "wqi"] %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  rename(spearman_r = ".") %>%
  filter(parameter != "wqi") %>%
  arrange(spearman_r)

wqi_corr

write.csv(wqi_corr, "results/wqi_parameter_correlation.csv", row.names = FALSE)

p_corr_wqi <- ggplot(wqi_corr, aes(x = reorder(parameter, spearman_r), y = spearman_r)) +
  geom_col(aes(fill = spearman_r > 0)) +
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values = c("tomato", "steelblue"), guide = "none") +
  labs(
    title = "Spearman Correlation Between Water Quality Parameters and WQI",
    x = "Parameter",
    y = "Spearman correlation coefficient"
  )

p_corr_wqi

ggsave("figures/wqi_parameter_correlation.png", p_corr_wqi, width = 8, height = 6, dpi = 300)





high_corr_pairs <- findCorrelation(
  cor(ml_data %>% select(all_of(raw_predictors)), use = "pairwise.complete.obs"),
  cutoff = 0.90,
  names = TRUE,
  exact = TRUE
)

high_corr_pairs

write.csv(
  data.frame(highly_correlated_variables = high_corr_pairs),
  "results/highly_correlated_variables.csv",
  row.names = FALSE
)




pca_input <- ml_data %>%
  select(all_of(raw_predictors)) %>%
  scale()

pca_res <- PCA(as.data.frame(pca_input), graph = FALSE)

eig <- get_eigenvalue(pca_res)

eig

write.csv(eig, "results/pca_eigenvalues.csv", row.names = FALSE)

p_scree <- fviz_eig(
  pca_res,
  addlabels = TRUE,
  barfill = "steelblue",
  barcolor = "black",
  linecolor = "red"
) +
  theme_classic() +
  labs(title = "PCA Scree Plot")

p_scree

ggsave("figures/pca_scree_plot.png", p_scree, width = 7, height = 5, dpi = 300)

p_pca_var <- fviz_pca_var(
  pca_res,
  col.var = "contrib",
  gradient.cols = c("blue", "orange", "red"),
  repel = TRUE
) +
  theme_bw() +
  labs(title = "PCA Variable Contribution Plot")

p_pca_var

ggsave("figures/pca_variable_contribution.png", p_pca_var, width = 8, height = 7, dpi = 300)

p_pca_ind <- fviz_pca_ind(
  pca_res,
  geom.ind = "point",
  col.ind = ml_data$wqi_class,
  palette = c("red", "orange", "darkgreen"),
  addEllipses = TRUE,
  legend.title = "WQI class"
) +
  theme_classic() +
  labs(title = "PCA Score Plot by WQI Class")

p_pca_ind

ggsave("figures/pca_samples_by_wqi_class.png", p_pca_ind, width = 8, height = 6, dpi = 300)

pca_loadings <- as.data.frame(pca_res$var$coord)
pca_loadings$parameter <- rownames(pca_loadings)

write.csv(pca_loadings, "results/pca_loadings.csv", row.names = FALSE)


##STEP 9 — Cluster analysis

cluster_input <- ml_data %>%
  select(all_of(raw_predictors)) %>%
  scale()

p_sil <- fviz_nbclust(
  cluster_input,
  kmeans,
  method = "silhouette"
) +
  theme_bw() +
  labs(title = "Optimal Number of Clusters: Silhouette Method")

p_sil

ggsave("figures/optimal_cluster_silhouette.png", p_sil, width = 7, height = 5, dpi = 300)

set.seed(123)

km3 <- kmeans(cluster_input, centers = 3, nstart = 50)

ml_data$cluster <- factor(km3$cluster)

cluster_summary <- ml_data %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    mean_wqi = mean(wqi),
    median_wqi = median(wqi),
    min_wqi = min(wqi),
    max_wqi = max(wqi),
    .groups = "drop"
  )

cluster_summary

write.csv(cluster_summary, "results/cluster_summary.csv", row.names = FALSE)

p_cluster <- fviz_cluster(
  km3,
  data = cluster_input,
  geom = "point",
  ellipse.type = "convex"
) +
  theme_bw() +
  labs(title = "K-means Clustering of Freshwater Samples")

p_cluster

ggsave("figures/kmeans_cluster_plot.png", p_cluster, width = 8, height = 6, dpi = 300)

p_cluster_wqi <- ggplot(ml_data, aes(x = cluster, y = wqi, fill = cluster)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.7) +
  theme_bw() +
  labs(
    title = "WQI Distribution Across Hydrochemical Clusters",
    x = "Cluster",
    y = "Water Quality Index"
  )

p_cluster_wqi

ggsave("figures/cluster_wqi_boxplot.png", p_cluster_wqi, width = 7, height = 5, dpi = 300)


##STEP 10 — ML data and common CV setup

model_data <- ml_data %>%
  select(all_of(raw_predictors), wqi)

model_data <- model_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

set.seed(123)

fold_index <- createMultiFolds(model_data$wqi, k = 10, times = 10)

ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  index = fold_index,
  savePredictions = "final",
  verboseIter = FALSE,
  allowParallel = FALSE
)



##STEP 11 — Train caret regression models


set.seed(123)

lm_model <- train(
  wqi ~ .,
  data = model_data,
  method = "lm",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  metric = "RMSE"
)

ridge_grid <- expand.grid(
  alpha = 0,
  lambda = 10^seq(-4, 2, length = 50)
)

set.seed(123)

ridge_model <- train(
  wqi ~ .,
  data = model_data,
  method = "glmnet",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneGrid = ridge_grid,
  metric = "RMSE"
)

lasso_grid <- expand.grid(
  alpha = 1,
  lambda = 10^seq(-4, 2, length = 50)
)

set.seed(123)

lasso_model <- train(
  wqi ~ .,
  data = model_data,
  method = "glmnet",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneGrid = lasso_grid,
  metric = "RMSE"
)

enet_grid <- expand.grid(
  alpha = seq(0.1, 0.9, by = 0.2),
  lambda = 10^seq(-4, 2, length = 20)
)

set.seed(123)

enet_model <- train(
  wqi ~ .,
  data = model_data,
  method = "glmnet",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneGrid = enet_grid,
  metric = "RMSE"
)

rf_grid <- expand.grid(
  mtry = c(3, 5, 7),
  splitrule = "variance",
  min.node.size = c(3, 5, 7)
)

set.seed(123)

rf_model <- train(
  wqi ~ .,
  data = model_data,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = rf_grid,
  importance = "permutation",
  metric = "RMSE"
)

set.seed(123)

svr_model <- train(
  wqi ~ .,
  data = model_data,
  method = "svmRadial",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneLength = 8,
  metric = "RMSE"
)



##STEP 12 — Model performance table for caret models 

models <- list(
  Linear_Model = lm_model,
  Ridge = ridge_model,
  Lasso = lasso_model,
  Elastic_Net = enet_model,
  Random_Forest = rf_model,
  SVR = svr_model
)

get_best_perf <- function(model_object, model_name) {
  
  res <- model_object$results
  best_row <- res[which.min(res$RMSE), , drop = FALSE]
  
  data.frame(
    model = model_name,
    RMSE_mean = best_row$RMSE,
    RMSE_sd = if ("RMSESD" %in% names(best_row)) best_row$RMSESD else NA,
    MAE_mean = if ("MAE" %in% names(best_row)) best_row$MAE else NA,
    MAE_sd = if ("MAESD" %in% names(best_row)) best_row$MAESD else NA,
    R2_mean = if ("Rsquared" %in% names(best_row)) best_row$Rsquared else NA,
    R2_sd = if ("RsquaredSD" %in% names(best_row)) best_row$RsquaredSD else NA
  )
}

model_perf <- bind_rows(
  Map(get_best_perf, models, names(models))
) %>%
  arrange(RMSE_mean)

model_perf

write.csv(model_perf, "results/model_performance_rmse.csv", row.names = FALSE)

p_model_perf <- ggplot(model_perf, aes(x = reorder(model, RMSE_mean), y = RMSE_mean)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Model Comparison Based on Cross-Validated RMSE",
    x = "Model",
    y = "RMSE"
  )

p_model_perf

ggsave("figures/caret_model_rmse_comparison.png", p_model_perf, width = 7, height = 5, dpi = 300)



## STEP 13 — Clean native XGBoost CV


x <- model_data %>%
  select(-wqi) %>%
  as.data.frame() %>%
  as.matrix()

y <- as.numeric(model_data$wqi)

set.seed(123)

folds <- createMultiFolds(y, k = 10, times = 10)

xgb_grid_clean <- expand.grid(
  eta_value = c(0.03, 0.05, 0.10),
  depth_value = c(2, 3),
  subsample_value = c(0.70, 0.90),
  colsample_value = c(0.70, 0.90),
  child_weight_value = c(1, 3),
  rounds_value = c(50, 100),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

xgb_cv_table <- data.frame()

for (i in seq_len(nrow(xgb_grid_clean))) {
  
  g <- xgb_grid_clean[i, ]
  
  all_obs <- c()
  all_pred <- c()
  
  for (fold_name in names(folds)) {
    
    train_index <- folds[[fold_name]]
    test_index <- setdiff(seq_along(y), train_index)
    
    dtrain <- xgb.DMatrix(
      data = x[train_index, , drop = FALSE],
      label = y[train_index]
    )
    
    dtest <- xgb.DMatrix(
      data = x[test_index, , drop = FALSE],
      label = y[test_index]
    )
    
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = as.numeric(g$eta_value),
      max_depth = as.integer(g$depth_value),
      subsample = as.numeric(g$subsample_value),
      colsample_bytree = as.numeric(g$colsample_value),
      min_child_weight = as.numeric(g$child_weight_value)
    )
    
    fit <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = as.integer(g$rounds_value),
      verbose = 0
    )
    
    pred <- predict(fit, dtest)
    
    all_obs <- c(all_obs, y[test_index])
    all_pred <- c(all_pred, pred)
  }
  
  xgb_RMSE_value <- sqrt(mean((all_obs - all_pred)^2))
  xgb_MAE_value <- mean(abs(all_obs - all_pred))
  xgb_R2_value <- cor(all_obs, all_pred)^2
  
  xgb_cv_table <- rbind(
    xgb_cv_table,
    data.frame(
      model = "XGBoost_direct",
      eta_value = as.numeric(g$eta_value),
      depth_value = as.integer(g$depth_value),
      subsample_value = as.numeric(g$subsample_value),
      colsample_value = as.numeric(g$colsample_value),
      child_weight_value = as.numeric(g$child_weight_value),
      rounds_value = as.integer(g$rounds_value),
      xgb_RMSE_value = xgb_RMSE_value,
      xgb_MAE_value = xgb_MAE_value,
      xgb_R2_value = xgb_R2_value
    )
  )
}

xgb_cv_table <- xgb_cv_table %>%
  arrange(xgb_RMSE_value)

xgb_cv_table

write.csv(xgb_cv_table, "results/xgboost_clean_cv_results.csv", row.names = FALSE)






##STEP 14 — Final XGBoost model and combine performance









best_xgb <- xgb_cv_table[1, , drop = FALSE]

dtrain_full <- xgb.DMatrix(data = x, label = y)

best_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = as.numeric(best_xgb$eta_value[1]),
  max_depth = as.integer(best_xgb$depth_value[1]),
  subsample = as.numeric(best_xgb$subsample_value[1]),
  colsample_bytree = as.numeric(best_xgb$colsample_value[1]),
  min_child_weight = as.numeric(best_xgb$child_weight_value[1])
)

final_xgb_model <- xgb.train(
  params = best_params,
  data = dtrain_full,
  nrounds = as.integer(best_xgb$rounds_value[1]),
  verbose = 0
)

xgb_perf <- data.frame(
  model = "XGBoost_direct",
  RMSE_mean = best_xgb$xgb_RMSE_value[1],
  RMSE_sd = NA,
  MAE_mean = best_xgb$xgb_MAE_value[1],
  MAE_sd = NA,
  R2_mean = best_xgb$xgb_R2_value[1],
  R2_sd = NA
)

combined_model_perf <- bind_rows(model_perf, xgb_perf) %>%
  arrange(RMSE_mean)

combined_model_perf

write.csv(combined_model_perf, "results/combined_model_performance.csv", row.names = FALSE)

p_combined_perf <- ggplot(combined_model_perf, aes(x = reorder(model, RMSE_mean), y = RMSE_mean)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Comparison of Machine Learning Models for WQI Prediction",
    x = "Model",
    y = "Cross-validated RMSE"
  )

p_combined_perf

ggsave("figures/combined_model_performance_rmse.png", p_combined_perf, width = 7, height = 5, dpi = 300)

best_model_overall <- combined_model_perf$model[1]
best_model_overall




##STEP 15 — Variable importance: RF and XGBoost



rf_importance <- varImp(rf_model)

rf_vip_df <- rf_importance$importance %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  arrange(desc(Overall))

rf_vip_df

write.csv(rf_vip_df, "results/random_forest_variable_importance.csv", row.names = FALSE)

p_rf_vip <- ggplot(rf_vip_df, aes(x = reorder(parameter, Overall), y = Overall)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Random Forest Variable Importance for WQI Prediction",
    x = "Parameter",
    y = "Importance"
  )

p_rf_vip

ggsave("figures/random_forest_variable_importance.png", p_rf_vip, width = 7, height = 6, dpi = 300)

xgb_importance <- xgb.importance(
  feature_names = colnames(x),
  model = final_xgb_model
)

xgb_importance

write.csv(xgb_importance, "results/xgboost_variable_importance.csv", row.names = FALSE)

png("figures/xgboost_variable_importance.png", width = 2200, height = 1700, res = 300)
xgb.plot.importance(xgb_importance, top_n = 15)
dev.off()




##STEP 16 — Partial dependence plots

top6_rf_vars <- rf_vip_df %>%
  slice(1:6) %>%
  pull(parameter)

top6_rf_vars

for (v in top6_rf_vars) {
  
  pd <- partial(
    object = rf_model,
    pred.var = v,
    train = model_data,
    grid.resolution = 20
  )
  
  p_pdp <- autoplot(pd, contour = FALSE) +
    theme_classic() +
    labs(
      title = paste("Partial Dependence of WQI on", v),
      x = v,
      y = "Predicted WQI"
    )
  
  print(p_pdp)
  
  ggsave(
    paste0("figures/pdp_", v, ".png"),
    p_pdp,
    width = 6,
    height = 5,
    dpi = 300
  )
}





##STEP 17 — Boruta feature selection

set.seed(123)

boruta_res <- Boruta(
  wqi ~ .,
  data = model_data,
  doTrace = 2,
  maxRuns = 500
)

boruta_fixed <- TentativeRoughFix(boruta_res)

boruta_df <- attStats(boruta_fixed) %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  arrange(desc(meanImp))

boruta_df

write.csv(boruta_df, "results/boruta_feature_selection.csv", row.names = FALSE)

png("figures/boruta_feature_selection.png", width = 2600, height = 1700, res = 300)
plot(
  boruta_fixed,
  las = 2,
  cex.axis = 0.8,
  main = "Boruta Feature Selection for WQI Prediction"
)
dev.off()

confirmed_vars <- boruta_df %>%
  filter(decision == "Confirmed") %>%
  pull(parameter)

confirmed_vars

write.csv(
  data.frame(confirmed_variables = confirmed_vars),
  "results/boruta_confirmed_variables.csv",
  row.names = FALSE
  
  
 ##STEP 18 — Reduced parameter optimization  
  
  ranked_vars <- rf_vip_df %>%
    arrange(desc(Overall)) %>%
    pull(parameter)
  
  ranked_vars
  
  reduced_results <- data.frame()
  
  for (k in 2:length(ranked_vars)) {
    
    selected_vars <- ranked_vars[1:k]
    
    temp_data <- model_data %>%
      select(all_of(selected_vars), wqi)
    
    set.seed(123)
    
    temp_model <- train(
      wqi ~ .,
      data = temp_data,
      method = "ranger",
      trControl = ctrl,
      tuneLength = 5,
      importance = "permutation",
      metric = "RMSE"
    )
    
    best_row <- temp_model$results[which.min(temp_model$results$RMSE), , drop = FALSE]
    
    reduced_results <- rbind(
      reduced_results,
      data.frame(
        number_of_parameters = k,
        selected_parameters = paste(selected_vars, collapse = ", "),
        RMSE = best_row$RMSE,
        MAE = if ("MAE" %in% names(best_row)) best_row$MAE else NA,
        R2 = best_row$Rsquared
      )
    )
  }
  
  reduced_results <- reduced_results %>%
    arrange(number_of_parameters)
  
  reduced_results
  
  write.csv(reduced_results, "results/reduced_parameter_optimization.csv", row.names = FALSE)
  
  p_reduced_rmse <- ggplot(reduced_results, aes(x = number_of_parameters, y = RMSE)) +
    geom_line(color = "red", linewidth = 1) +
    geom_point(size = 2, color = "black") +
    theme_bw() +
    labs(
      title = "Monitoring Optimization: RMSE vs Number of Parameters",
      x = "Number of monitored parameters",
      y = "Cross-validated RMSE"
    )
  
  p_reduced_rmse
  
  ggsave("figures/reduced_parameter_rmse.png", p_reduced_rmse, width = 7, height = 5, dpi = 300)
  
  best_rmse <- min(reduced_results$RMSE)
  
  optimized_set <- reduced_results %>%
    filter(RMSE <= best_rmse * 1.05) %>%
    arrange(number_of_parameters) %>%
    slice(1)
  
  optimized_set
  
  write.csv(optimized_set, "results/optimized_monitoring_parameter_set.csv", row.names = FALSE)
)



##STEP 19 — Classification model

class_data <- ml_data %>%
  select(all_of(raw_predictors), wqi_class)

levels(class_data$wqi_class)

class_ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  classProbs = TRUE,
  savePredictions = "final",
  allowParallel = FALSE
)

set.seed(123)

rf_class_model <- train(
  wqi_class ~ .,
  data = class_data,
  method = "ranger",
  trControl = class_ctrl,
  tuneLength = 10,
  importance = "permutation",
  metric = "Accuracy"
)

rf_class_model

class_pred <- rf_class_model$pred

if (!is.null(rf_class_model$bestTune)) {
  for (param in names(rf_class_model$bestTune)) {
    class_pred <- class_pred[class_pred[[param]] == rf_class_model$bestTune[[param]], ]
  }
}

conf_mat <- confusionMatrix(class_pred$pred, class_pred$obs)

conf_mat

capture.output(conf_mat, file = "results/classification_confusion_matrix.txt")

class_vip <- varImp(rf_class_model)

class_vip_df <- class_vip$importance %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  arrange(desc(Overall))

class_vip_df

write.csv(class_vip_df, "results/classification_variable_importance.csv", row.names = FALSE)

p_class_vip <- ggplot(class_vip_df, aes(x = reorder(parameter, Overall), y = Overall)) +
  geom_col(fill = "purple") +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Variable Importance for WQI Class Prediction",
    x = "Parameter",
    y = "Importance"
  )

p_class_vip

ggsave("figures/classification_variable_importance.png", p_class_vip, width = 7, height = 6, dpi = 300)



##STEP 20 — Classification probability risk ranking


class_prob_results <- class_pred %>%
  select(
    rowIndex,
    obs,
    pred,
    Low_quality,
    Moderate_quality,
    Better_quality
  ) %>%
  mutate(
    sample_id = ml_data$sample_id[rowIndex]
  ) %>%
  group_by(sample_id) %>%
  summarise(
    observed_class = first(obs),
    predicted_class_majority = names(sort(table(pred), decreasing = TRUE))[1],
    mean_Low_quality = mean(Low_quality, na.rm = TRUE),
    mean_Moderate_quality = mean(Moderate_quality, na.rm = TRUE),
    mean_Better_quality = mean(Better_quality, na.rm = TRUE),
    .groups = "drop"
  )

class_prob_results

write.csv(
  class_prob_results,
  "results/class_prediction_probabilities.csv",
  row.names = FALSE
)

classification_risk <- class_prob_results %>%
  arrange(desc(mean_Low_quality))

classification_risk

write.csv(
  classification_risk,
  "results/classification_risk_ranking.csv",
  row.names = FALSE
)

p_class_risk <- ggplot(
  classification_risk,
  aes(x = reorder(sample_id, mean_Low_quality), y = mean_Low_quality)
) +
  geom_col(fill = "tomato") +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Probability-Based Ranking of Low-Quality Water Samples",
    x = "Sample ID",
    y = "Mean predicted probability of Low-quality class"
  )

p_class_risk

ggsave(
  "figures/classification_low_quality_probability_ranking.png",
  p_class_risk,
  width = 8,
  height = 10,
  dpi = 300
)

top10_classification_risk <- classification_risk %>%
  slice(1:10)

top10_classification_risk

write.csv(
  top10_classification_risk,
  "results/top10_classification_high_risk_samples.csv",
  row.names = FALSE
)









##STEP 21 — Bootstrap uncertainty analysis




set.seed(123)

B <- 500
n <- nrow(model_data)

boot_predictions <- matrix(NA, nrow = n, ncol = B)

for (b in 1:B) {
  
  boot_idx <- sample(1:n, size = n, replace = TRUE)
  boot_train <- model_data[boot_idx, ]
  
  boot_model <- ranger(
    wqi ~ .,
    data = boot_train,
    num.trees = 500,
    mtry = floor(sqrt(length(raw_predictors))),
    min.node.size = 5
  )
  
  boot_predictions[, b] <- predict(
    boot_model,
    data = model_data %>% select(-wqi)
  )$predictions
}

uncertainty_df <- ml_data %>%
  mutate(
    pred_mean = rowMeans(boot_predictions),
    pred_lower = apply(boot_predictions, 1, quantile, 0.025),
    pred_upper = apply(boot_predictions, 1, quantile, 0.975),
    uncertainty_width = pred_upper - pred_lower
  ) %>%
  arrange(pred_mean)

uncertainty_df

write.csv(uncertainty_df, "results/bootstrap_prediction_uncertainty.csv", row.names = FALSE)

p_uncertainty <- ggplot(uncertainty_df, aes(x = reorder(sample_id, pred_mean), y = pred_mean)) +
  geom_point(color = "blue", size = 2) +
  geom_errorbar(aes(ymin = pred_lower, ymax = pred_upper), width = 0.2, color = "gray40") +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Bootstrap-Based Prediction Uncertainty for WQI",
    x = "Sample ID",
    y = "Predicted WQI with 95% uncertainty interval"
  )

p_uncertainty

ggsave("figures/bootstrap_prediction_uncertainty.png", p_uncertainty, width = 8, height = 10, dpi = 300)




##STEP 22 — Regression-based sample risk ranking

rf_pred_all <- predict(rf_model, newdata = model_data %>% select(-wqi))

risk_ranking <- ml_data %>%
  mutate(
    predicted_wqi = rf_pred_all,
    prediction_error = wqi - predicted_wqi
  ) %>%
  arrange(predicted_wqi) %>%
  select(
    sample_id, wqi, predicted_wqi, prediction_error,
    turbidity, ec, tds, ph, th, ca, mg, na, fe, no3, cl, so4
  )

risk_ranking

write.csv(risk_ranking, "results/sample_risk_ranking.csv", row.names = FALSE)

top10_risk <- risk_ranking %>%
  slice(1:10)

top10_risk

write.csv(top10_risk, "results/top10_high_risk_samples.csv", row.names = FALSE)

p_risk <- ggplot(risk_ranking, aes(x = reorder(sample_id, predicted_wqi), y = predicted_wqi)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Sample-Level Risk Ranking Based on Predicted WQI",
    x = "Sample ID",
    y = "Predicted WQI"
  )

p_risk

ggsave("figures/sample_risk_ranking.png", p_risk, width = 8, height = 10, dpi = 300)



##STEP 23 — Agreement among importance methods

rf_top10 <- rf_vip_df %>%
  slice(1:10) %>%
  transmute(parameter, RF_rank = row_number())

xgb_top10 <- xgb_importance %>%
  as.data.frame() %>%
  slice(1:10) %>%
  transmute(parameter = Feature, XGB_rank = row_number())

boruta_top10 <- boruta_df %>%
  arrange(desc(meanImp)) %>%
  slice(1:10) %>%
  transmute(parameter, Boruta_rank = row_number(), Boruta_decision = decision)

importance_agreement <- full_join(rf_top10, xgb_top10, by = "parameter") %>%
  full_join(boruta_top10, by = "parameter") %>%
  arrange(coalesce(RF_rank, 999), coalesce(XGB_rank, 999), coalesce(Boruta_rank, 999))

importance_agreement

write.csv(importance_agreement, "results/feature_importance_agreement.csv", row.names = FALSE)








##2. Fixed decision optimization analysis
 It creates a fresh CV setup and uses tryCatch() so one failed model will not stop the full loop.
##2.1 Create feature ranking

 
 
 # ============================================================
 # 2.1 Feature ranking for monitoring optimization
 # ============================================================
 
 set.seed(123)
 
 ctrl_opt <- trainControl(
   method = "repeatedcv",
   number = 10,
   repeats = 10,
   savePredictions = "final",
   verboseIter = FALSE,
   allowParallel = FALSE
 )
 
 if (!exists("rf_vip_df")) {
   
   rf_grid_opt <- expand.grid(
     mtry = c(3, 5, 7),
     splitrule = "variance",
     min.node.size = c(3, 5, 7)
   )
   
   rf_model <- train(
     wqi ~ .,
     data = model_data,
     method = "ranger",
     trControl = ctrl_opt,
     tuneGrid = rf_grid_opt,
     importance = "permutation",
     metric = "RMSE"
   )
   
   rf_importance <- varImp(rf_model)
   
   rf_vip_df <- rf_importance$importance %>%
     as.data.frame() %>%
     rownames_to_column("parameter") %>%
     arrange(desc(Overall))
 }
 
 rf_vip_df
 
 ranked_vars <- rf_vip_df %>%
   arrange(desc(Overall)) %>%
   pull(parameter)
 
 ranked_vars




 # ============================================================
 # 2.2 Reduced monitoring parameter optimization
 # ============================================================
 
 reduced_results <- data.frame()
 
 for (k in 2:length(ranked_vars)) {
   
   selected_vars <- ranked_vars[1:k]
   
   temp_data <- model_data %>%
     select(all_of(selected_vars), wqi)
   
   set.seed(123)
   
   fit_k <- tryCatch(
     {
       train(
         wqi ~ .,
         data = temp_data,
         method = "ranger",
         trControl = ctrl_opt,
         tuneLength = 5,
         importance = "permutation",
         metric = "RMSE"
       )
     },
     error = function(e) {
       message("Model failed for k = ", k, ": ", e$message)
       return(NULL)
     }
   )
   
   if (!is.null(fit_k)) {
     
     best_row <- fit_k$results[which.min(fit_k$results$RMSE), , drop = FALSE]
     
     reduced_results <- rbind(
       reduced_results,
       data.frame(
         number_of_parameters = k,
         selected_parameters = paste(selected_vars, collapse = ", "),
         RMSE = best_row$RMSE,
         MAE = if ("MAE" %in% names(best_row)) best_row$MAE else NA,
         R2 = best_row$Rsquared
       )
     )
   }
 }
 
 reduced_results <- reduced_results %>%
   arrange(number_of_parameters)
 
 reduced_results
 
 write.csv(reduced_results, "results/reduced_parameter_optimization.csv", row.names = FALSE)
 
 
 
 
 
 
 
 
 
 
 ##2.3 Plot decision optimization result
 
 p_reduced_rmse <- ggplot(reduced_results, aes(x = number_of_parameters, y = RMSE)) +
   geom_line(color = "red", linewidth = 1) +
   geom_point(size = 2.5, color = "black") +
   theme_classic() +
   labs(
     title = "Decision Optimization: Prediction Error vs Monitoring Effort",
     x = "Number of monitored parameters",
     y = "Cross-validated RMSE"
   )
 
 p_reduced_rmse
 
 ggsave("figures/decision_optimization_rmse_vs_parameters.png", p_reduced_rmse, width = 7, height = 5, dpi = 300)
 
 p_reduced_r2 <- ggplot(reduced_results, aes(x = number_of_parameters, y = R2)) +
   geom_line(color = "darkgreen", linewidth = 1) +
   geom_point(size = 2.5, color = "black") +
   theme_classic() +
   labs(
     title = "Decision Optimization: R² vs Monitoring Effort",
     x = "Number of monitored parameters",
     y = "Cross-validated R²"
   )
 
 p_reduced_r2
 
 ggsave("figures/decision_optimization_r2_vs_parameters.png", p_reduced_r2, width = 7, height = 5, dpi = 300)
 
 
 
##2.4 Select optimized monitoring set 
 
 
 best_rmse <- min(reduced_results$RMSE, na.rm = TRUE)
 
 optimized_set <- reduced_results %>%
   filter(RMSE <= best_rmse * 1.05) %>%
   arrange(number_of_parameters) %>%
   slice(1)
 
 optimized_set
 
 write.csv(optimized_set, "results/optimized_monitoring_parameter_set.csv", row.names = FALSE)
 
 
 
 
 
 
 # ============================================================
 # 2.5 Pareto-front monitoring optimization
 # ============================================================
 
 pareto_results <- reduced_results %>%
   arrange(number_of_parameters) %>%
   mutate(
     best_rmse_so_far = cummin(RMSE),
     pareto_front = RMSE <= best_rmse_so_far
   )
 
 pareto_front <- pareto_results %>%
   filter(pareto_front == TRUE)
 
 pareto_front
 
 write.csv(pareto_front, "results/pareto_front_monitoring_optimization.csv", row.names = FALSE)
 
 p_pareto <- ggplot(reduced_results, aes(x = number_of_parameters, y = RMSE)) +
   geom_point(color = "gray40", size = 2.2) +
   geom_line(color = "gray60") +
   geom_point(data = pareto_front, color = "red", size = 3) +
   geom_line(data = pareto_front, color = "red", linewidth = 1) +
   theme_classic() +
   labs(
     title = "Pareto Front for Monitoring Optimization",
     x = "Number of monitored parameters",
     y = "Cross-validated RMSE"
   )
 
 p_pareto
 
 ggsave("figures/pareto_front_monitoring_optimization.png", p_pareto, width = 7, height = 5, dpi = 300)
 
 
 
 
 
 # ============================================================
 # 3.1 Ensure final XGBoost model exists
 # ============================================================
 
 x <- model_data %>%
   select(-wqi) %>%
   as.data.frame() %>%
   as.matrix()
 
 y <- as.numeric(model_data$wqi)
 
 dtrain_full <- xgb.DMatrix(data = x, label = y)
 
 if (!exists("final_xgb_model")) {
   
   best_xgb <- xgb_cv_table[1, , drop = FALSE]
   
   best_params <- list(
     objective = "reg:squarederror",
     eval_metric = "rmse",
     eta = as.numeric(best_xgb$eta_value[1]),
     max_depth = as.integer(best_xgb$depth_value[1]),
     subsample = as.numeric(best_xgb$subsample_value[1]),
     colsample_bytree = as.numeric(best_xgb$colsample_value[1]),
     min_child_weight = as.numeric(best_xgb$child_weight_value[1])
   )
   
   final_xgb_model <- xgb.train(
     params = best_params,
     data = dtrain_full,
     nrounds = as.integer(best_xgb$rounds_value[1]),
     verbose = 0
   )
 }
 
 
 
 
 # ============================================================
 # 3.2 XGBoost SHAP global importance
 # ============================================================
 
 # ============================================================
 # SHAP ANALYSIS - CLEAN FIXED VERSION
 # ============================================================
 
 library(xgboost)
 library(tidyverse)
 library(ggplot2)
 
 if (!dir.exists("results")) dir.create("results")
 if (!dir.exists("figures")) dir.create("figures")
 
 # Prepare matrix again safely
 x <- model_data %>%
   select(-wqi) %>%
   as.data.frame() %>%
   as.matrix()
 
 y <- as.numeric(model_data$wqi)
 
 # Ensure column names exist
 colnames(x) <- names(model_data %>% select(-wqi))
 
 dtrain_full <- xgb.DMatrix(data = x, label = y)
 
 # Check final model exists
 if (!exists("final_xgb_model")) {
   stop("final_xgb_model not found. Please run final XGBoost model training first.")
 }
 
 dim(x)
 colnames(x)
 
 
 
 
 
 
 
 # ============================================================
 # Generate SHAP matrix
 # ============================================================
 
 shap_matrix <- predict(
   final_xgb_model,
   dtrain_full,
   predcontrib = TRUE
 )
 
 shap_df <- as.data.frame(shap_matrix)
 
 # Check column names
 names(shap_df)
 dim(shap_df)
 
 
 
 # ============================================================
 # Remove bias/intercept column safely
 # ============================================================
 
 # Find bias column by name
 bias_col <- grep("bias", names(shap_df), ignore.case = TRUE, value = TRUE)
 
 # If bias column name is found, remove it
 if (length(bias_col) > 0) {
   
   shap_values_only <- shap_df %>%
     select(-all_of(bias_col))
   
 } else {
   
   # If no bias column name found, assume last column is bias
   shap_values_only <- shap_df[, 1:ncol(x), drop = FALSE]
   colnames(shap_values_only) <- colnames(x)
 }
 
 # Make sure SHAP columns match predictors
 names(shap_values_only)
 dim(shap_values_only)
 
 
 
 
 
 # ============================================================
 # Global SHAP importance
 # ============================================================
 
 shap_importance <- data.frame(
   parameter = names(shap_values_only),
   mean_abs_shap = apply(abs(shap_values_only), 2, mean)
 ) %>%
   arrange(desc(mean_abs_shap))
 
 shap_importance
 
 write.csv(
   shap_importance,
   "results/xgboost_shap_global_importance.csv",
   row.names = FALSE
 )
 
 p_shap_global <- ggplot(
   shap_importance,
   aes(x = reorder(parameter, mean_abs_shap), y = mean_abs_shap)
 ) +
   geom_col(fill = "darkblue") +
   coord_flip() +
   theme_classic() +
   labs(
     title = "Global SHAP Importance for WQI Prediction",
     x = "Parameter",
     y = "Mean absolute SHAP value"
   )
 
 p_shap_global
 
 ggsave(
   "figures/xgboost_shap_global_importance.png",
   p_shap_global,
   width = 7,
   height = 6,
   dpi = 300
 )
 
 
 
 
 
 
 # ============================================================
 # Drinking-water standard exceedance analysis
 # Bangladesh DPHE-based limits; adjust if needed
 # ============================================================
 
 library(tidyverse)
 
 if (!dir.exists("results")) dir.create("results")
 if (!dir.exists("figures")) dir.create("figures")
 
 standards <- data.frame(
   parameter = c("ph", "turbidity", "tds", "th", "ca", "mg", "na",
                 "k", "fe", "mn", "no3", "no2", "cl", "f", "so4", "po4"),
   lower_limit = c(6.5, NA, NA, 200, NA, NA, NA,
                   NA, NA, NA, NA, NA, NA, NA, NA, NA),
   upper_limit = c(8.5, 10, 1000, 500, 75, 35, 200,
                   12, 1.0, 0.1, 10, 1, 600, 1, 400, 6)
 )
 
 exceedance_long <- ml_data %>%
   select(sample_id, all_of(standards$parameter), wqi) %>%
   pivot_longer(
     cols = all_of(standards$parameter),
     names_to = "parameter",
     values_to = "value"
   ) %>%
   left_join(standards, by = "parameter") %>%
   mutate(
     below_limit = ifelse(!is.na(lower_limit) & value < lower_limit, TRUE, FALSE),
     above_limit = ifelse(!is.na(upper_limit) & value > upper_limit, TRUE, FALSE),
     exceeds_standard = below_limit | above_limit
   )
 
 exceedance_summary <- exceedance_long %>%
   group_by(parameter) %>%
   summarise(
     n_exceed = sum(exceeds_standard, na.rm = TRUE),
     percent_exceed = 100 * mean(exceeds_standard, na.rm = TRUE),
     min_value = min(value, na.rm = TRUE),
     median_value = median(value, na.rm = TRUE),
     mean_value = mean(value, na.rm = TRUE),
     max_value = max(value, na.rm = TRUE),
     .groups = "drop"
   ) %>%
   arrange(desc(percent_exceed))
 
 exceedance_summary
 
 write.csv(exceedance_summary, "results/drinking_standard_exceedance_summary.csv", row.names = FALSE)
 write.csv(exceedance_long, "results/sample_parameter_standard_exceedance.csv", row.names = FALSE)
 
 
 
 # ============================================================
 # SHAP summary-style plot for top variables
 # ============================================================
 
 top_shap_vars <- shap_importance %>%
   slice(1:8) %>%
   pull(parameter)
 
 top_shap_vars
 
 shap_long <- shap_values_only %>%
   mutate(sample_id = ml_data$sample_id) %>%
   pivot_longer(
     cols = all_of(top_shap_vars),
     names_to = "parameter",
     values_to = "shap_value"
   )
 
 feature_long <- model_data %>%
   select(all_of(top_shap_vars)) %>%
   mutate(sample_id = ml_data$sample_id) %>%
   pivot_longer(
     cols = all_of(top_shap_vars),
     names_to = "parameter",
     values_to = "feature_value"
   )
 
 shap_plot_data <- shap_long %>%
   left_join(feature_long, by = c("sample_id", "parameter"))
 
 p_shap_summary <- ggplot(
   shap_plot_data,
   aes(x = shap_value, y = parameter, color = feature_value)
 ) +
   geom_point(alpha = 0.75, size = 2) +
   scale_color_gradient(low = "blue", high = "red") +
   theme_classic() +
   labs(
     title = "SHAP Summary Plot for Top Predictors",
     x = "SHAP value",
     y = "Parameter",
     color = "Feature value"
   )
 
 p_shap_summary
 
 ggsave(
   "figures/xgboost_shap_summary_top_variables.png",
   p_shap_summary,
   width = 8,
   height = 6,
   dpi = 300
 )
 
 # ============================================================
 # Local SHAP explanation for worst WQI sample
 # ============================================================
 
 worst_index <- which.min(ml_data$wqi)
 worst_sample_id <- ml_data$sample_id[worst_index]
 
 worst_sample_id
 ml_data$wqi[worst_index]
 
 local_shap <- shap_values_only[worst_index, , drop = FALSE] %>%
   pivot_longer(
     cols = everything(),
     names_to = "parameter",
     values_to = "shap_value"
   ) %>%
   arrange(desc(abs(shap_value))) %>%
   slice(1:10)
 
 local_shap
 
 write.csv(
   local_shap,
   "results/local_shap_worst_sample.csv",
   row.names = FALSE
 )
 
 p_local_shap <- ggplot(
   local_shap,
   aes(x = reorder(parameter, shap_value), y = shap_value)
 ) +
   geom_col(aes(fill = shap_value > 0)) +
   coord_flip() +
   theme_classic() +
   scale_fill_manual(values = c("tomato", "steelblue"), guide = "none") +
   labs(
     title = paste("Local SHAP Explanation for Worst Sample:", worst_sample_id),
     x = "Parameter",
     y = "SHAP contribution"
   )
 
 p_local_shap
 
 ggsave(
   "figures/local_shap_worst_sample.png",
   p_local_shap,
   width = 7,
   height = 5,
   dpi = 300
 )
 
 
 
 
 
 
 
 # ============================================================
 # SHAP dependence plots for top 6 variables
 # ============================================================
 
 top6_shap_vars <- shap_importance %>%
   slice(1:6) %>%
   pull(parameter)
 
 for (v in top6_shap_vars) {
   
   temp_df <- data.frame(
     feature_value = model_data[[v]],
     shap_value = shap_values_only[[v]],
     wqi = model_data$wqi,
     sample_id = ml_data$sample_id
   )
   
   p_dep <- ggplot(temp_df, aes(x = feature_value, y = shap_value)) +
     geom_point(aes(color = wqi), size = 2.5, alpha = 0.8) +
     scale_color_gradient(low = "red", high = "darkgreen") +
     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
     theme_classic() +
     labs(
       title = paste("SHAP Dependence Plot:", v),
       x = v,
       y = "SHAP value",
       color = "WQI"
     )
   
   print(p_dep)
   
   ggsave(
     paste0("figures/shap_dependence_", v, ".png"),
     p_dep,
     width = 6,
     height = 5,
     dpi = 300
   )
 }
 
 


 
 # ============================================================
 # 14. Agreement among importance methods
 # ============================================================
 
 rf_top10 <- rf_vip_df %>%
   slice(1:10) %>%
   transmute(parameter, RF_rank = row_number())
 
 shap_top10 <- shap_importance %>%
   slice(1:10) %>%
   transmute(parameter, SHAP_rank = row_number())
 
 if (exists("boruta_df")) {
   
   boruta_top10 <- boruta_df %>%
     arrange(desc(meanImp)) %>%
     slice(1:10) %>%
     transmute(
       parameter,
       Boruta_rank = row_number(),
       Boruta_decision = decision
     )
   
   importance_agreement <- full_join(rf_top10, shap_top10, by = "parameter") %>%
     full_join(boruta_top10, by = "parameter")
   
 } else {
   
   importance_agreement <- full_join(rf_top10, shap_top10, by = "parameter")
 }
 
 importance_agreement <- importance_agreement %>%
   arrange(
     coalesce(RF_rank, 999),
     coalesce(SHAP_rank, 999)
   )
 
 importance_agreement
 
 write.csv(
   importance_agreement,
   "results/feature_importance_agreement.csv",
   row.names = FALSE
 )
 
 
 
 
 # ============================================================
 # Drinking-water standard exceedance analysis
 # Bangladesh DPHE-based limits; adjust if needed
 # ============================================================
 
 library(tidyverse)
 
 if (!dir.exists("results")) dir.create("results")
 if (!dir.exists("figures")) dir.create("figures")
 
 standards <- data.frame(
   parameter = c("ph", "turbidity", "tds", "th", "ca", "mg", "na",
                 "k", "fe", "mn", "no3", "no2", "cl", "f", "so4", "po4"),
   lower_limit = c(6.5, NA, NA, 200, NA, NA, NA,
                   NA, NA, NA, NA, NA, NA, NA, NA, NA),
   upper_limit = c(8.5, 10, 1000, 500, 75, 35, 200,
                   12, 1.0, 0.1, 10, 1, 600, 1, 400, 6)
 )
 
 exceedance_long <- ml_data %>%
   select(sample_id, all_of(standards$parameter), wqi) %>%
   pivot_longer(
     cols = all_of(standards$parameter),
     names_to = "parameter",
     values_to = "value"
   ) %>%
   left_join(standards, by = "parameter") %>%
   mutate(
     below_limit = ifelse(!is.na(lower_limit) & value < lower_limit, TRUE, FALSE),
     above_limit = ifelse(!is.na(upper_limit) & value > upper_limit, TRUE, FALSE),
     exceeds_standard = below_limit | above_limit
   )
 
 exceedance_summary <- exceedance_long %>%
   group_by(parameter) %>%
   summarise(
     n_exceed = sum(exceeds_standard, na.rm = TRUE),
     percent_exceed = 100 * mean(exceeds_standard, na.rm = TRUE),
     min_value = min(value, na.rm = TRUE),
     median_value = median(value, na.rm = TRUE),
     mean_value = mean(value, na.rm = TRUE),
     max_value = max(value, na.rm = TRUE),
     .groups = "drop"
   ) %>%
   arrange(desc(percent_exceed))
 
 exceedance_summary
 
 write.csv(exceedance_summary, "results/drinking_standard_exceedance_summary.csv", row.names = FALSE)
 write.csv(exceedance_long, "results/sample_parameter_standard_exceedance.csv", row.names = FALSE) 
 
 
 
 
 
 
 
 
 p_exceedance <- ggplot(exceedance_summary, aes(x = reorder(parameter, percent_exceed), y = percent_exceed)) +
   geom_col(fill = "firebrick") +
   coord_flip() +
   theme_classic() +
   labs(
     title = "Percentage of Samples Exceeding Drinking-Water Standards",
     x = "Parameter",
     y = "Samples exceeding standard (%)"
   )
 
 p_exceedance
 
 ggsave("figures/drinking_standard_exceedance_percentage.png", p_exceedance, width = 7, height = 5, dpi = 300)
 
 
 
 
 
 
 
 
 
 
 
 # ============================================================
 # Sample-level contamination burden score
 # ============================================================
 
 contamination_burden <- exceedance_long %>%
   group_by(sample_id) %>%
   summarise(
     n_parameters_tested = n(),
     n_exceedances = sum(exceeds_standard, na.rm = TRUE),
     exceedance_percentage = 100 * n_exceedances / n_parameters_tested,
     exceeded_parameters = paste(parameter[exceeds_standard], collapse = ", "),
     .groups = "drop"
   ) %>%
   left_join(ml_data %>% select(sample_id, wqi), by = "sample_id") %>%
   arrange(desc(n_exceedances), wqi)
 
 contamination_burden
 
 write.csv(contamination_burden, "results/sample_contamination_burden.csv", row.names = FALSE)
 ``
 
 
 
 
 
 p_burden <- ggplot(contamination_burden, aes(x = reorder(sample_id, n_exceedances), y = n_exceedances)) +
   geom_col(fill = "darkred") +
   coord_flip() +
   theme_classic() +
   labs(
     title = "Sample-Level Contamination Burden",
     x = "Sample ID",
     y = "Number of exceeded parameters"
   )
 
 p_burden
 
 ggsave("figures/sample_contamination_burden.png", p_burden, width = 8, height = 10, dpi = 300)
 
 
 
 
 
 
 
 
 
 
 # ============================================================
 # Irrigation suitability indices
 # Units conversion: mg/L to meq/L
 # ============================================================
 
 irrigation_df <- ml_data %>%
   mutate(
     ca_meq = ca / 20.04,
     mg_meq = mg / 12.15,
     na_meq = na / 23.00,
     k_meq = k / 39.10,
     
     SAR = na_meq / sqrt((ca_meq + mg_meq) / 2),
     Sodium_percent = ((na_meq + k_meq) / (ca_meq + mg_meq + na_meq + k_meq)) * 100,
     Kelly_ratio = na_meq / (ca_meq + mg_meq),
     Magnesium_hazard = (mg_meq / (ca_meq + mg_meq)) * 100,
     Permeability_index = ((na_meq + sqrt(0)) / (ca_meq + mg_meq + na_meq)) * 100
   ) %>%
   select(sample_id, wqi, SAR, Sodium_percent, Kelly_ratio, Magnesium_hazard, Permeability_index)
 
 irrigation_df
 
 write.csv(irrigation_df, "results/irrigation_suitability_indices.csv", row.names = FALSE)
 
 
 
 
 
 
 
 
 irrigation_class <- irrigation_df %>%
   mutate(
     SAR_class = case_when(
       SAR < 10 ~ "Excellent",
       SAR < 18 ~ "Good",
       SAR < 26 ~ "Doubtful",
       TRUE ~ "Unsuitable"
     ),
     Na_percent_class = case_when(
       Sodium_percent < 20 ~ "Excellent",
       Sodium_percent < 40 ~ "Good",
       Sodium_percent < 60 ~ "Permissible",
       Sodium_percent < 80 ~ "Doubtful",
       TRUE ~ "Unsuitable"
     ),
     Kelly_class = ifelse(Kelly_ratio <= 1, "Suitable", "Unsuitable"),
     MH_class = ifelse(Magnesium_hazard <= 50, "Suitable", "Unsuitable")
   )
 
 irrigation_class
 
 write.csv(irrigation_class, "results/irrigation_suitability_classification.csv", row.names = FALSE)
 
 
 
 
 
 p_sar <- ggplot(irrigation_class, aes(x = reorder(sample_id, SAR), y = SAR, fill = SAR_class)) +
   geom_col() +
   coord_flip() +
   theme_classic() +
   labs(
     title = "Irrigation Suitability Based on Sodium Adsorption Ratio",
     x = "Sample ID",
     y = "SAR"
   )
 
 p_sar
 
 ggsave("figures/irrigation_sar_ranking.png", p_sar, width = 8, height = 10, dpi = 300)
 
 
 
 
 
 
 # ============================================================
 # Scenario/intervention sensitivity analysis using RF model
 # ============================================================
 
 library(tidyverse)
 
 scenario_vars <- head(rf_vip_df$parameter, 8)
 
 scenario_results <- data.frame()
 
 baseline_pred <- predict(rf_model, newdata = model_data %>% select(-wqi))
 
 for (v in scenario_vars) {
   
   scenario_data <- model_data %>% select(-wqi)
   
   # Reduce parameter by 10%, 20%, 30%
   for (reduction in c(0.10, 0.20, 0.30)) {
     
     scenario_temp <- scenario_data
     scenario_temp[[v]] <- scenario_temp[[v]] * (1 - reduction)
     
     scenario_pred <- predict(rf_model, newdata = scenario_temp)
     
     scenario_results <- rbind(
       scenario_results,
       data.frame(
         parameter = v,
         reduction_percent = reduction * 100,
         mean_baseline_predicted_wqi = mean(baseline_pred),
         mean_scenario_predicted_wqi = mean(scenario_pred),
         mean_wqi_change = mean(scenario_pred - baseline_pred)
       )
     )
   }
 }
 
 scenario_results <- scenario_results %>%
   arrange(desc(mean_wqi_change))
 
 scenario_results
 
 write.csv(scenario_results, "results/scenario_intervention_sensitivity.csv", row.names = FALSE)
 
 
 
 
 
 
 
 p_scenario <- ggplot(scenario_results, aes(x = reorder(parameter, mean_wqi_change), y = mean_wqi_change, fill = factor(reduction_percent))) +
   geom_col(position = "dodge") +
   coord_flip() +
   theme_classic() +
   labs(
     title = "Scenario-Based Sensitivity of Predicted WQI",
     x = "Parameter reduced",
     y = "Mean predicted WQI change",
     fill = "Reduction (%)"
   )
 
 p_scenario
 
 ggsave("figures/scenario_intervention_sensitivity.png", p_scenario, width = 8, height = 6, dpi = 300)
 
 
 
 
 
 # ============================================================
 # Learning curve analysis
 # ============================================================
 
 set.seed(123)
 
 train_sizes <- c(0.35, 0.50, 0.65, 0.80, 1.00)
 n_repeat <- 100
 
 learning_results <- data.frame()
 
 for (s in train_sizes) {
   
   for (r in 1:n_repeat) {
     
     idx <- sample(1:nrow(model_data), size = floor(s * nrow(model_data)), replace = FALSE)
     temp_train <- model_data[idx, ]
     
     fit <- ranger(
       wqi ~ .,
       data = temp_train,
       num.trees = 500,
       mtry = floor(sqrt(length(raw_predictors))),
       min.node.size = 5
     )
     
     pred_all <- predict(fit, data = model_data %>% select(-wqi))$predictions
     
     rmse_all <- sqrt(mean((model_data$wqi - pred_all)^2))
     mae_all <- mean(abs(model_data$wqi - pred_all))
     r2_all <- cor(model_data$wqi, pred_all)^2
     
     learning_results <- rbind(
       learning_results,
       data.frame(
         train_fraction = s,
         train_n = length(idx),
         repeat_id = r,
         RMSE = rmse_all,
         MAE = mae_all,
         R2 = r2_all
       )
     )
   }
 }
 
 learning_summary <- learning_results %>%
   group_by(train_fraction, train_n) %>%
   summarise(
     RMSE_mean = mean(RMSE),
     RMSE_sd = sd(RMSE),
     R2_mean = mean(R2),
     R2_sd = sd(R2),
     .groups = "drop"
   )
 
 learning_summary
 
 write.csv(learning_results, "results/learning_curve_raw.csv", row.names = FALSE)
 write.csv(learning_summary, "results/learning_curve_summary.csv", row.names = FALSE)
 
 
 
 
 
 
 
 
 
 p_learning <- ggplot(learning_summary, aes(x = train_n, y = RMSE_mean)) +
   geom_line(color = "darkblue", linewidth = 1) +
   geom_point(size = 2.5) +
   geom_errorbar(aes(ymin = RMSE_mean - RMSE_sd, ymax = RMSE_mean + RMSE_sd), width = 1) +
   theme_classic() +
   labs(
     title = "Learning Curve for WQI Prediction",
     x = "Training sample size",
     y = "Mean RMSE"
   )
 
 p_learning
 
 ggsave("figures/learning_curve_rmse.png", p_learning, width = 7, height = 5, dpi = 300)
 
 
 
 
 
 
 # ============================================================
 # External validation template
 # Run only if external dataset available
 # ============================================================
 
 # external_raw <- read_excel("C:/path/to/external_dataset.xlsx")
 
 # Rename external dataset columns to match:
 # sample_id, turbidity, ec, tds, ph, th, ca, mg, na, k, fe, mn,
 # nh3, no2, no3, cl, f, so4, po4, wqi
 
 # external_data <- external_raw %>%
 #   select(all_of(raw_predictors), wqi) %>%
 #   mutate(across(everything(), ~ as.numeric(.))) %>%
 #   mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
 
 # common_predictors <- intersect(raw_predictors, names(external_data))
 
 # train_common <- model_data %>%
 #   select(all_of(common_predictors), wqi)
 
 # external_common <- external_data %>%
 #   select(all_of(common_predictors), wqi)
 
 # set.seed(123)
 
 # rf_external_model <- ranger(
 #   wqi ~ .,
 #   data = train_common,
 #   num.trees = 500,
 #   mtry = floor(sqrt(length(common_predictors))),
 #   min.node.size = 5
 # )
 
 # external_pred <- predict(
 #   rf_external_model,
 #   data = external_common %>% select(-wqi)
 # )$predictions
 
 # external_validation_metrics <- data.frame(
 #   RMSE = sqrt(mean((external_common$wqi - external_pred)^2)),
 #   MAE = mean(abs(external_common$wqi - external_pred)),
 #   R2 = cor(external_common$wqi, external_pred)^2
 # )
 
 # external_validation_metrics
 
 # write.csv(external_validation_metrics, "results/external_validation_metrics.csv", row.names = FALSE)
 
 
 
 
 
 
 
 
 
 
 
 
 # ============================================================
 # FIX XGBoost best model selection
 # ============================================================
 
 xgb_cv_table <- xgb_cv_table %>%
   arrange(xgb_RMSE_value)
 
 best_xgb <- xgb_cv_table[1, , drop = FALSE]
 best_xgb
 
 dtrain_full <- xgb.DMatrix(data = x, label = y)
 
 best_params <- list(
   objective = "reg:squarederror",
   eval_metric = "rmse",
   eta = as.numeric(best_xgb$eta_value[1]),
   max_depth = as.integer(best_xgb$depth_value[1]),
   subsample = as.numeric(best_xgb$subsample_value[1]),
   colsample_bytree = as.numeric(best_xgb$colsample_value[1]),
   min_child_weight = as.numeric(best_xgb$child_weight_value[1])
 )
 
 final_xgb_model <- xgb.train(
   params = best_params,
   data = dtrain_full,
   nrounds = as.integer(best_xgb$rounds_value[1]),
   verbose = 0
 )
 
 xgb_perf <- data.frame(
   model = "XGBoost_direct",
   RMSE_mean = best_xgb$xgb_RMSE_value[1],
   RMSE_sd = NA,
   MAE_mean = best_xgb$xgb_MAE_value[1],
   MAE_sd = NA,
   R2_mean = best_xgb$xgb_R2_value[1],
   R2_sd = NA
 )
 
 combined_model_perf_fixed <- bind_rows(model_perf, xgb_perf) %>%
   arrange(RMSE_mean)
 
 combined_model_perf_fixed
 
 write.csv(
   combined_model_perf_fixed,
   "results/combined_model_performance_FIXED.csv",
   row.names = FALSE
 )
 
 p_combined_fixed <- ggplot(
   combined_model_perf_fixed,
   aes(x = reorder(model, RMSE_mean), y = RMSE_mean)
 ) +
   geom_col(fill = "steelblue") +
   coord_flip() +
   theme_classic() +
   labs(
     x = "Model",
     y = "Cross-validated RMSE"
   )
 
 p_combined_fixed
 
 ggsave(
   "figures/combined_model_performance_rmse_FIXED.png",
   p_combined_fixed,
   width = 7,
   height = 5,
   dpi = 300
 )
 ``
 
 
 
 
 
 # ============================================================
 # Recompute SHAP after fixed final_xgb_model
 # ============================================================
 
 shap_matrix <- predict(
   final_xgb_model,
   dtrain_full,
   predcontrib = TRUE
 )
 
 shap_df <- as.data.frame(shap_matrix)
 
 # remove intercept/bias safely
 shap_values_only <- shap_df[, 1:ncol(x), drop = FALSE]
 colnames(shap_values_only) <- colnames(x)
 
 shap_importance_fixed <- data.frame(
   parameter = names(shap_values_only),
   mean_abs_shap = apply(abs(shap_values_only), 2, mean)
 ) %>%
   arrange(desc(mean_abs_shap))
 
 shap_importance_fixed
 
 write.csv(
   shap_importance_fixed,
   "results/xgboost_shap_global_importance_FIXED.csv",
   row.names = FALSE
 )
 
 p_shap_global_fixed <- ggplot(
   shap_importance_fixed,
   aes(x = reorder(parameter, mean_abs_shap), y = mean_abs_shap)
 ) +
   geom_col(fill = "darkblue") +
   coord_flip() +
   theme_classic() +
   labs(
     x = "Parameter",
     y = "Mean absolute SHAP value"
   )
 

 
 ggsave(
   "figures/xgboost_shap_global_importance_FIXED.png",
   p_shap_global_fixed,
   width = 7,
   height = 6,
   dpi = 300
 )
 `
 
 
 
 worst_index <- which.min(ml_data$wqi)
 worst_sample_id <- ml_data$sample_id[worst_index]
 
 local_shap_fixed <- shap_values_only[worst_index, , drop = FALSE] %>%
   pivot_longer(
     cols = everything(),
     names_to = "parameter",
     values_to = "shap_value"
   ) %>%
   arrange(desc(abs(shap_value))) %>%
   slice(1:10)
 
 local_shap_fixed
 
 write.csv(
   local_shap_fixed,
   "results/local_shap_worst_sample_FIXED.csv",
   row.names = FALSE
 )
 
 
 
 
 
 
 
 
 
 
 # ============================================================
 # Complete reduced-parameter optimization up to all variables
 # ============================================================
 
 ranked_vars <- rf_vip_df %>%
   arrange(desc(Overall)) %>%
   pull(parameter)
 
 ctrl_opt <- trainControl(
   method = "repeatedcv",
   number = 10,
   repeats = 10,
   savePredictions = "final",
   verboseIter = FALSE,
   allowParallel = FALSE
 )
 
 reduced_results_full <- data.frame()
 
 for (k in 2:length(ranked_vars)) {
   
   selected_vars <- ranked_vars[1:k]
   
   temp_data <- model_data %>%
     select(all_of(selected_vars), wqi)
   
   rf_grid_simple <- expand.grid(
     mtry = pmax(1, floor(sqrt(k))),
     splitrule = "variance",
     min.node.size = 5
   )
   
   set.seed(123)
   
   fit_k <- train(
     wqi ~ .,
     data = temp_data,
     method = "ranger",
     trControl = ctrl_opt,
     tuneGrid = rf_grid_simple,
     importance = "permutation",
     metric = "RMSE"
   )
   
   best_row <- fit_k$results[which.min(fit_k$results$RMSE), , drop = FALSE]
   
   reduced_results_full <- rbind(
     reduced_results_full,
     data.frame(
       number_of_parameters = k,
       selected_parameters = paste(selected_vars, collapse = ", "),
       RMSE = best_row$RMSE,
       MAE = if ("MAE" %in% names(best_row)) best_row$MAE else NA,
       R2 = best_row$Rsquared
     )
   )
 }
 
 reduced_results_full
 
 write.csv(
   reduced_results_full,
   "results/reduced_parameter_optimization_FULL.csv",
   row.names = FALSE
 )
 
 best_rmse_full <- min(reduced_results_full$RMSE, na.rm = TRUE)
 
 optimized_set_full <- reduced_results_full %>%
   filter(RMSE <= best_rmse_full * 1.05) %>%
   arrange(number_of_parameters) %>%
   slice(1)
 
 optimized_set_full
 
 write.csv(
   optimized_set_full,
   "results/optimized_monitoring_parameter_set_FULL.csv",
   row.names = FALSE
 )
 
 
 
 worst_index <- which.min(ml_data$wqi)
 worst_sample_id <- ml_data$sample_id[worst_index]
 
 local_shap_fixed <- shap_values_only[worst_index, , drop = FALSE] %>%
   pivot_longer(
     cols = everything(),
     names_to = "parameter",
     values_to = "shap_value"
   ) %>%
   arrange(desc(abs(shap_value))) %>%
   slice(1:10)
 
 local_shap_fixed
 
 write.csv(
   local_shap_fixed,
   "results/local_shap_worst_sample_FIXED.csv",
   row.names = FALSE
 )
 
 
 
 # ============================================================
 # Complete reduced-parameter optimization up to all variables
 # ============================================================
 
 ranked_vars <- rf_vip_df %>%
   arrange(desc(Overall)) %>%
   pull(parameter)
 
 ctrl_opt <- trainControl(
   method = "repeatedcv",
   number = 10,
   repeats = 10,
   savePredictions = "final",
   verboseIter = FALSE,
   allowParallel = FALSE
 )
 
 reduced_results_full <- data.frame()
 
 for (k in 2:length(ranked_vars)) {
   
   selected_vars <- ranked_vars[1:k]
   
   temp_data <- model_data %>%
     select(all_of(selected_vars), wqi)
   
   rf_grid_simple <- expand.grid(
     mtry = pmax(1, floor(sqrt(k))),
     splitrule = "variance",
     min.node.size = 5
   )
   
   set.seed(123)
   
   fit_k <- train(
     wqi ~ .,
     data = temp_data,
     method = "ranger",
     trControl = ctrl_opt,
     tuneGrid = rf_grid_simple,
     importance = "permutation",
     metric = "RMSE"
   )
   
   best_row <- fit_k$results[which.min(fit_k$results$RMSE), , drop = FALSE]
   
   reduced_results_full <- rbind(
     reduced_results_full,
     data.frame(
       number_of_parameters = k,
       selected_parameters = paste(selected_vars, collapse = ", "),
       RMSE = best_row$RMSE,
       MAE = if ("MAE" %in% names(best_row)) best_row$MAE else NA,
       R2 = best_row$Rsquared
     )
   )
 }
 
 reduced_results_full
 
 write.csv(
   reduced_results_full,
   "results/reduced_parameter_optimization_FULL.csv",
   row.names = FALSE
 )
 
 best_rmse_full <- min(reduced_results_full$RMSE, na.rm = TRUE)
 
 optimized_set_full <- reduced_results_full %>%
   filter(RMSE <= best_rmse_full * 1.05) %>%
   arrange(number_of_parameters) %>%
   slice(1)
 
 optimized_set_full
 
 write.csv(
   optimized_set_full,
   "results/optimized_monitoring_parameter_set_FULL.csv",
   row.names = FALSE
 )
 
 
 
 # ============================================================
 # Permutation test for model significance
 # ============================================================
 
 ctrl_opt <- trainControl(
   method = "repeatedcv",
   number = 10,
   repeats = 5,
   savePredictions = "final",
   verboseIter = FALSE,
   allowParallel = FALSE
 )
 
 set.seed(123)
 
 n_perm <- 100
 
 true_rmse <- min(rf_model$results$RMSE)
 
 perm_rmse <- c()
 
 for (i in 1:n_perm) {
   
   perm_data <- model_data
   perm_data$wqi <- sample(perm_data$wqi)
   
   rf_grid_perm <- expand.grid(
     mtry = floor(sqrt(length(raw_predictors))),
     splitrule = "variance",
     min.node.size = 5
   )
   
   set.seed(123 + i)
   
   perm_fit <- train(
     wqi ~ .,
     data = perm_data,
     method = "ranger",
     trControl = ctrl_opt,
     tuneGrid = rf_grid_perm,
     importance = "none",
     metric = "RMSE"
   )
   
   perm_rmse[i] <- min(perm_fit$results$RMSE)
 }
 
 p_value_perm <- mean(perm_rmse <= true_rmse)
 
 perm_summary <- data.frame(
   true_rmse = true_rmse,
   mean_permuted_rmse = mean(perm_rmse),
   sd_permuted_rmse = sd(perm_rmse),
   permutation_p_value = p_value_perm
 )
 
 perm_summary
 
 write.csv(
   perm_summary,
   "results/permutation_test_model_significance.csv",
   row.names = FALSE
 )
 
 p_perm <- ggplot(data.frame(perm_rmse = perm_rmse), aes(x = perm_rmse)) +
   geom_histogram(bins = 20, fill = "gray70", color = "black") +
   geom_vline(xintercept = true_rmse, color = "red", linewidth = 1.2) +
   theme_classic() +
   labs(
     x = "RMSE under permuted WQI",
     y = "Frequency"
   )
 
 p_perm
 
 ggsave(
   "figures/permutation_test_model_significance.png",
   p_perm,
   width = 7,
   height = 5,
   dpi = 300
 )
 
 
 
 
 
 
 # ============================================================
 # Multicollinearity robustness model
 # ============================================================
 
 high_corr_remove <- high_corr_pairs
 robust_predictors <- setdiff(raw_predictors, high_corr_remove)
 
 robust_model_data <- ml_data %>%
   select(all_of(robust_predictors), wqi) %>%
   mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
 
 set.seed(123)
 
 rf_grid_robust <- expand.grid(
   mtry = floor(sqrt(length(robust_predictors))),
   splitrule = "variance",
   min.node.size = 5
 )
 
 rf_robust_model <- train(
   wqi ~ .,
   data = robust_model_data,
   method = "ranger",
   trControl = ctrl_opt,
   tuneGrid = rf_grid_robust,
   importance = "permutation",
   metric = "RMSE"
 )
 
 robust_best <- rf_robust_model$results[which.min(rf_robust_model$results$RMSE), , drop = FALSE]
 
 robust_summary <- data.frame(
   model = "RF_without_highly_correlated_predictors",
   removed_variables = paste(high_corr_remove, collapse = ", "),
   retained_variables = paste(robust_predictors, collapse = ", "),
   RMSE = robust_best$RMSE,
   MAE = if ("MAE" %in% names(robust_best)) robust_best$MAE else NA,
   R2 = robust_best$Rsquared
 )
 
 robust_summary
 
 write.csv(
   robust_summary,
   "results/multicollinearity_robustness_model.csv",
   row.names = FALSE
 )
 
 
 
 
 
 # ============================================================
 # FIX 1: XGBoost best model selection
 # ============================================================
 
 xgb_cv_table <- xgb_cv_table %>%
   arrange(xgb_RMSE_value)
 
 best_xgb <- xgb_cv_table[1, , drop = FALSE]
 best_xgb
 
 dtrain_full <- xgb.DMatrix(data = x, label = y)
 
 best_params <- list(
   objective = "reg:squarederror",
   eval_metric = "rmse",
   eta = as.numeric(best_xgb$eta_value[1]),
   max_depth = as.integer(best_xgb$depth_value[1]),
   subsample = as.numeric(best_xgb$subsample_value[1]),
   colsample_bytree = as.numeric(best_xgb$colsample_value[1]),
   min_child_weight = as.numeric(best_xgb$child_weight_value[1])
 )
 
 final_xgb_model <- xgb.train(
   params = best_params,
   data = dtrain_full,
   nrounds = as.integer(best_xgb$rounds_value[1]),
   verbose = 0
 )
 
 xgb_perf <- data.frame(
   model = "XGBoost_direct",
   RMSE_mean = best_xgb$xgb_RMSE_value[1],
   RMSE_sd = NA,
   MAE_mean = best_xgb$xgb_MAE_value[1],
   MAE_sd = NA,
   R2_mean = best_xgb$xgb_R2_value[1],
   R2_sd = NA
 )
 
 combined_model_perf_fixed <- bind_rows(model_perf, xgb_perf) %>%
   arrange(RMSE_mean)
 
 combined_model_perf_fixed
 
 write.csv(
   combined_model_perf_fixed,
   "results/combined_model_performance_FIXED.csv",
   row.names = FALSE
 )
 
 p_combined_fixed <- ggplot(
   combined_model_perf_fixed,
   aes(x = reorder(model, RMSE_mean), y = RMSE_mean)
 ) +
   geom_col(fill = "steelblue") +
   coord_flip() +
   theme_classic() +
   labs(
     x = "Model",
     y = "Cross-validated RMSE"
   )
 
 p_combined_fixed
 
 ggsave(
   "figures/combined_model_performance_rmse_FIXED.png",
   p_combined_fixed,
   width = 7,
   height = 5,
   dpi = 300
 )
 ``
 
 
 
 
 
 
 
 
 
 # ============================================================
 # FIX 2: Recompute SHAP after corrected XGBoost model
 # ============================================================
 
 shap_matrix <- predict(
   final_xgb_model,
   dtrain_full,
   predcontrib = TRUE
 )
 
 shap_df <- as.data.frame(shap_matrix)
 
 # remove intercept/bias column safely
 shap_values_only <- shap_df[, 1:ncol(x), drop = FALSE]
 colnames(shap_values_only) <- colnames(x)
 
 shap_importance_fixed <- data.frame(
   parameter = names(shap_values_only),
   mean_abs_shap = apply(abs(shap_values_only), 2, mean)
 ) %>%
   arrange(desc(mean_abs_shap))
 
 shap_importance_fixed
 
 write.csv(
   shap_importance_fixed,
   "results/xgboost_shap_global_importance_FIXED.csv",
   row.names = FALSE
 )
 
 p_shap_global_fixed <- ggplot(
   shap_importance_fixed,
   aes(x = reorder(parameter, mean_abs_shap), y = mean_abs_shap)
 ) +
   geom_col(fill = "darkblue") +
   coord_flip() +
   theme_classic() +
   labs(
     x = "Parameter",
     y = "Mean absolute SHAP value"
   )
 
 p_shap_global_fixed
 
 ggsave(
   "figures/xgboost_shap_global_importance_FIXED.png",
   p_shap_global_fixed,
   width = 7,
   height = 6,
   dpi = 300
 )
 
 # Local SHAP for worst sample
 worst_index <- which.min(ml_data$wqi)
 worst_sample_id <- ml_data$sample_id[worst_index]
 
 local_shap_fixed <- shap_values_only[worst_index, , drop = FALSE] %>%
   pivot_longer(
     cols = everything(),
     names_to = "parameter",
     values_to = "shap_value"
   ) %>%
   arrange(desc(abs(shap_value))) %>%
   slice(1:10)
 
 local_shap_fixed
 
 write.csv(
   local_shap_fixed,
   "results/local_shap_worst_sample_FIXED.csv",
   row.names = FALSE
 )
 
 
 
 
 
 
 
 
 
 
 
 
 
 # ============================================================
 # FIX 3: Complete reduced-parameter optimization up to all variables
 # ============================================================
 
 ranked_vars <- rf_vip_df %>%
   arrange(desc(Overall)) %>%
   pull(parameter)
 
 ctrl_opt <- trainControl(
   method = "repeatedcv",
   number = 10,
   repeats = 10,
   savePredictions = "final",
   verboseIter = FALSE,
   allowParallel = FALSE
 )
 
 reduced_results_full <- data.frame()
 
 for (k in 2:length(ranked_vars)) {
   
   selected_vars <- ranked_vars[1:k]
   
   temp_data <- model_data %>%
     select(all_of(selected_vars), wqi)
   
   rf_grid_simple <- expand.grid(
     mtry = pmax(1, floor(sqrt(k))),
     splitrule = "variance",
     min.node.size = 5
   )
   
   set.seed(123)
   
   fit_k <- train(
     wqi ~ .,
     data = temp_data,
     method = "ranger",
     trControl = ctrl_opt,
     tuneGrid = rf_grid_simple,
     importance = "permutation",
     metric = "RMSE"
   )
   
   best_row <- fit_k$results[which.min(fit_k$results$RMSE), , drop = FALSE]
   
   reduced_results_full <- rbind(
     reduced_results_full,
     data.frame(
       number_of_parameters = k,
       selected_parameters = paste(selected_vars, collapse = ", "),
       RMSE = best_row$RMSE,
       MAE = if ("MAE" %in% names(best_row)) best_row$MAE else NA,
       R2 = best_row$Rsquared
     )
   )
 }
 
 reduced_results_full
 
 write.csv(
   reduced_results_full,
   "results/reduced_parameter_optimization_FULL.csv",
   row.names = FALSE
 )
 
 best_rmse_full <- min(reduced_results_full$RMSE, na.rm = TRUE)
 
 optimized_set_full <- reduced_results_full %>%
   filter(RMSE <= best_rmse_full * 1.05) %>%
   arrange(number_of_parameters) %>%
   slice(1)
 
 optimized_set_full
 
 write.csv(
   optimized_set_full,
   "results/optimized_monitoring_parameter_set_FULL.csv",
   row.names = FALSE
 )
 
 
 
 
 
 
 
 
 
 
 
 pareto_results_full <- reduced_results_full %>%
   arrange(number_of_parameters) %>%
   mutate(
     best_rmse_so_far = cummin(RMSE),
     pareto_front = RMSE <= best_rmse_so_far
   )
 
 pareto_front_full <- pareto_results_full %>%
   filter(pareto_front == TRUE)
 
 pareto_front_full
 
 write.csv(
   pareto_front_full,
   "results/pareto_front_monitoring_optimization_FULL.csv",
   row.names = FALSE
 )
 
 p_pareto_full <- ggplot(reduced_results_full, aes(x = number_of_parameters, y = RMSE)) +
   geom_point(color = "gray40", size = 2.2) +
   geom_line(color = "gray60") +
   geom_point(data = pareto_front_full, color = "red", size = 3) +
   geom_line(data = pareto_front_full, color = "red", linewidth = 1) +
   theme_classic() +
   labs(
     x = "Number of monitored parameters",
     y = "Cross-validated RMSE"
   )
 
 
 
 
 # ============================================================
 # FIX 4A: Permutation test for model significance
 # ============================================================
 
 ctrl_opt_perm <- trainControl(
   method = "repeatedcv",
   number = 10,
   repeats = 5,
   savePredictions = "final",
   verboseIter = FALSE,
   allowParallel = FALSE
 )
 
 set.seed(123)
 
 n_perm <- 100
 true_rmse <- min(rf_model$results$RMSE)
 
 perm_rmse <- c()
 
 for (i in 1:n_perm) {
   
   perm_data <- model_data
   perm_data$wqi <- sample(perm_data$wqi)
   
   rf_grid_perm <- expand.grid(
     mtry = floor(sqrt(length(raw_predictors))),
     splitrule = "variance",
     min.node.size = 5
   )
   
   set.seed(123 + i)
   
   perm_fit <- train(
     wqi ~ .,
     data = perm_data,
     method = "ranger",
     trControl = ctrl_opt_perm,
     tuneGrid = rf_grid_perm,
     importance = "none",
     metric = "RMSE"
   )
   
   perm_rmse[i] <- min(perm_fit$results$RMSE)
 }
 
 p_value_perm <- mean(perm_rmse <= true_rmse)
 
 perm_summary <- data.frame(
   true_rmse = true_rmse,
   mean_permuted_rmse = mean(perm_rmse),
   sd_permuted_rmse = sd(perm_rmse),
   permutation_p_value = p_value_perm
 )
 
 perm_summary
 
 write.csv(
   perm_summary,
   "results/permutation_test_model_significance.csv",
   row.names = FALSE
 )
 
 p_perm <- ggplot(data.frame(perm_rmse = perm_rmse), aes(x = perm_rmse)) +
   geom_histogram(bins = 20, fill = "gray70", color = "black") +
   geom_vline(xintercept = true_rmse, color = "red", linewidth = 1.2) +
   theme_classic() +
   labs(
     x = "RMSE under permuted WQI",
     y = "Frequency"
   )
 
 p_perm
 
 ggsave(
   "figures/permutation_test_model_significance.png",
   p_perm,
   width = 7,
   height = 5,
   dpi = 300
 )
 
 p_pareto_full
 
 ggsave(
   "figures/pareto_front_monitoring_optimization_FULL.png",
   p_pareto_full,
   width = 7,
   height = 5,
   dpi = 300
 )
 `
 
 
 
 
 
 
 
 
 
 # ============================================================
 # FIX 4B: Multicollinearity robustness model
 # ============================================================
 
 high_corr_remove <- high_corr_pairs
 robust_predictors <- setdiff(raw_predictors, high_corr_remove)
 
 robust_model_data <- ml_data %>%
   select(all_of(robust_predictors), wqi) %>%
   mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
 
 set.seed(123)
 
 ctrl_opt_robust <- trainControl(
   method = "repeatedcv",
   number = 10,
   repeats = 5,
   savePredictions = "final",
   verboseIter = FALSE,
   allowParallel = FALSE
 )
 
 rf_grid_robust <- expand.grid(
   mtry = floor(sqrt(length(robust_predictors))),
   splitrule = "variance",
   min.node.size = 5
 )
 
 rf_robust_model <- train(
   wqi ~ .,
   data = robust_model_data,
   method = "ranger",
   trControl = ctrl_opt_robust,
   tuneGrid = rf_grid_robust,
   importance = "permutation",
   metric = "RMSE"
 )
 
 robust_best <- rf_robust_model$results[which.min(rf_robust_model$results$RMSE), , drop = FALSE]
 
 robust_summary <- data.frame(
   model = "RF_without_highly_correlated_predictors",
   removed_variables = paste(high_corr_remove, collapse = ", "),
   retained_variables = paste(robust_predictors, collapse = ", "),
   RMSE = robust_best$RMSE,
   MAE = if ("MAE" %in% names(robust_best)) robust_best$MAE else NA,
   R2 = robust_best$Rsquared
 )
 
 robust_summary
 
 write.csv(
   robust_summary,
   "results/multicollinearity_robustness_model.csv",
   row.names = FALSE
 )