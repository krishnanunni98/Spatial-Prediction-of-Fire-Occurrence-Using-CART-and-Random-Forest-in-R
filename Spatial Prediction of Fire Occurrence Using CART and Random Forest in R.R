# ==========================================================
# Spatial Prediction of Fire Occurrence Using
# CART and Random Forest Models
# ==========================================================

rm(list = ls())

# ----------------------------------------------------------
# 1. Load required packages
# ----------------------------------------------------------

library(sf)
library(terra)
library(rpart)
library(rpart.plot)
library(randomForest)
library(dismo)
library(ggplot2)
library(tidyterra)
library(tidyverse)

# ----------------------------------------------------------
# 2. Load fire occurrence data
# ----------------------------------------------------------

fire_logit <- read.csv2("fire_logit.csv") %>%
  drop_na()

fire_logit <- st_as_sf(
  fire_logit,
  coords = c("X_INDEX", "Y_INDEX"),
  crs = 25830
)

# Response variable
vdep <- fire_logit$logit_1_0

# Coordinates for raster extraction
fire_coords <- st_coordinates(fire_logit)

# ----------------------------------------------------------
# 3. Load raster predictor layers
# ----------------------------------------------------------

files <- list.files(
  "variables_fires",
  pattern = "\\.asc$",
  full.names = TRUE
)

rasters <- rast(files)

names(rasters) <- tools::file_path_sans_ext(
  basename(files)
)

# ----------------------------------------------------------
# 4. Extract raster values at sample locations
# ----------------------------------------------------------

vindep <- terra::extract(rasters, fire_coords)

vindep <- vindep[, -1]

regression <- data.frame(
  vdep,
  vindep
)

regression <- na.omit(regression)

# ----------------------------------------------------------
# 5. Split into calibration and validation datasets
# ----------------------------------------------------------

set.seed(123)

n <- nrow(regression)

cal_size <- floor(0.75 * n)

cal_index <- sample(
  seq_len(n),
  size = cal_size
)

regression_cal <- regression[cal_index, ]
regression_val <- regression[-cal_index, ]

# ==========================================================
# CART MODEL
# ==========================================================

# ----------------------------------------------------------
# 6. Fit classification tree
# ----------------------------------------------------------

cart_model <- rpart(
  vdep ~ .,
  data = regression_cal,
  method = "class",
  control = rpart.control(
    minsplit = 5,
    cp = 0.003
  )
)

# ----------------------------------------------------------
# 7. Prune tree
# ----------------------------------------------------------

cart_pruned <- prune(
  cart_model,
  cp = cart_model$cptable[
    which.min(cart_model$cptable[, "xerror"]),
    "CP"
  ]
)

# ----------------------------------------------------------
# 8. CART validation
# ----------------------------------------------------------

cart_pred <- predict(
  cart_pruned,
  regression_val,
  type = "class"
)

confusion_matrix <- table(
  Observed = regression_val$vdep,
  Predicted = cart_pred
)

print(confusion_matrix)

# ----------------------------------------------------------
# 9. CART spatial prediction
# ----------------------------------------------------------

names(rasters) <- names(regression)[-1]

cart_map <- predict(
  rasters,
  cart_pruned,
  type = "vector",
  index = 1
)

plot(
  cart_map,
  main = "CART Spatial Prediction"
)

# ==========================================================
# RANDOM FOREST MODEL
# ==========================================================

# ----------------------------------------------------------
# 10. Fit Random Forest
# ----------------------------------------------------------

rf_model <- randomForest(
  vdep ~ .,
  data = regression_cal,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

print(rf_model)

# ----------------------------------------------------------
# 11. Evaluate Random Forest
# ----------------------------------------------------------

rf_eval <- evaluate(
  p = regression_val[
    regression_val$vdep == 1,
  ],
  a = regression_val[
    regression_val$vdep == 0,
  ],
  model = rf_model
)

# ROC curve
plot(
  rf_eval,
  "ROC"
)

# AUC value
rf_eval@auc

# ----------------------------------------------------------
# 12. Variable importance
# ----------------------------------------------------------

varImpPlot(rf_model)

# ----------------------------------------------------------
# 13. Random Forest spatial prediction
# ----------------------------------------------------------

rf_map <- predict(
  rasters,
  rf_model,
  type = "response",
  index = 1
)

plot(
  rf_map,
  main = "Random Forest Spatial Prediction"
)

# ----------------------------------------------------------
# 14. Save outputs
# ----------------------------------------------------------

writeRaster(
  cart_map,
  "cart_prediction.tif",
  overwrite = TRUE
)

writeRaster(
  rf_map,
  "random_forest_prediction.tif",
  overwrite = TRUE
)