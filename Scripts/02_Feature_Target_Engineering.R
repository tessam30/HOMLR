# Chapter 3 - Feature and Target Engineering

pacman::p_load("visdat", "caret", "recipes", "tidyverse")

# Log outliers to make response variable more normal ----
transformed_response <- log(ames_train$Sale_Price)

# Create a blueprint to apply this processing later
ames_recipe <- recipe(Sale_Price ~ ., data = ames_train) %>% 
                      step_log(all_outcomes())

ames_recipe

# log1p(-0.5) # can be used to transform very small values

# Use a box-cox transformation to find an appropriate transformation that will get
# transform variable as close as possible to a normal distribution

y <- log(10)
exp(y)

y <- forecast::BoxCox(10, lambda)

# Missing data ----
sum(is.na(AmesHousing::ames_raw))

# Plot missing data by variable
AmesHousing::ames_raw %>% 
  is.na() %>% 
  reshape2::melt() %>% 
  ggplot(aes(Var2, Var1, fill = value)) + 
  geom_raster() +
  coord_flip() +
  scale_y_continuous(NULL, expand = c(0, 0)) +
  scale_fill_grey(name = "", 
                  labels = c("Present", "Missing")) +
  theme(axis.text.y = element_text(size = 6))

AmesHousing::ames_raw %>% 
  filter(is.na(`Garage Type`)) %>% 
  select(contains('Garage'))

# Can also use vis_dat to quickly glance at data
vis_miss(AmesHousing::ames_raw, cluster = TRUE) 

# Imputation ----
ames_recipe %>% 
  step_medianimpute(Gr_Liv_Area)

ames_recipe %>% 
  step_knnimpute(all_predictors(), neighbors = 6)

ames_recipe %>% 
  step_bagimpute(all_predictors())

# Near 0 variance predictors ----
caret::nearZeroVar(ames_train, saveMetrics = TRUE) %>% 
  tibble::rowid_to_column() %>% 
  filter(nzv)

# Numeric Feature Engineering ----
recipe(Sale_Price ~ ., data = ames_train) %>% 
  step_YeoJohnson(all_numeric()) # for skeweness normalizing

# Can also standardize to center everything, get mean zero, unit variance
# Standardize across both training and test datasets
ames_recipe %>% 
  step_center(all_numeric(), -all_outcomes()) %>% 
  step_scale(all_numeric(), -all_outcomes())

# Categorical feature engineering ----
# For lumpy categories, may have to do some engineering
count(ames_train, Neighborhood) %>% arrange(n)
count(ames_train, Screen_Porch) %>% arrange(n)

# Lumping
lumping <- recipe(Sale_Price ~ ., data = ames_train) %>% 
  step_other(Neighborhood, threshold = 0.01,
             other = "Other") %>% 
  step_other(Screen_Porch, threshold = 0.1,
             other = ">0")

# Now apply this blueprint
apply_2_training <- prep(lumping, training = ames_train) %>% 
  bake(ames_train)

count(apply_2_training, Neighborhood) %>% 
  arrange(n) %>% print(n = Inf)

# Dummy Encoding
recipe(Sale_Price ~ ., data = ames_train) %>% 
  step_dummy(all_nominal(), one_hot = TRUE)


# Application ----
# Steps: 1) Near-zero variance removed
# 2) Ordinal encode quality based features
# 3) Center and scale
# 4) dimension reduction

blueprint <- recipe(Sale_Price ~ ., data = ames_train) %>%
  step_nzv(all_nominal())  %>%
  step_integer(matches("Qual|Cond|QC|Qu")) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_pca(all_numeric(), -all_outcomes())
blueprint


prepare <- prep(blueprint, training = ames_train)

baked_train <- bake(prepare, new_data = ames_train)
baked_test <- bake(prepare, new_data = ames_test)
baked_train


# within each resample iteration we want to apply prep() and bake() to our resample training and validation data.
blueprint <- recipe(Sale_Price ~ ., data = ames_train) %>%
  step_nzv(all_nominal()) %>%
  step_integer(matches("Qual|Cond|QC|Qu")) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)


# Specify resampling plan
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5
)

# Construct grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

# Tune a knn model using grid search
knn_fit2 <- train(
  blueprint, 
  data = ames_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "RMSE"
)

knn_fit2
ggplot(knn_fit2)
