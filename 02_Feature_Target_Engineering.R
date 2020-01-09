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
