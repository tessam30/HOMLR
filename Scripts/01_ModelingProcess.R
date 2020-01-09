# From Chapter 2 of book HOML

# for latest R version of h2o
#install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))

pacman::p_load("tidyverse", "rsample", "AmesHousing", "caret", "h2o", "dslabs", "splines")

ames <- AmesHousing::make_ames()
attrition <- rsample::attrition
#mnist <- dslabs::read_mnist()
url <- "https://koalaverse.github.io/homlr/data/my_basket.csv"

# Set up h2o
h2o.no_progress()
h2o.init()

# Start with the Ames housing data and convert to h2o data
ames.h2o <- as.h2o(ames)

churn <- rsample::attrition %>% 
  mutate_if(is.ordered, .funs = factor, ordered = FALSE)
churn.h2o <- as.h2o(churn)

# Sampling for training / test data
# using h2o this becomes
split_2 <- h2o.splitFrame(ames.h2o, ratios = 0.7, seed = 123)
train_4 <- split_2[[1]]
test_4  <- split_2[[2]]

# For stratified sampling use the rsample package and specify a strata variabl

table(churn$Attrition) %>% prop.table()

set.seed(123)
split_strat <- initial_split(churn, prop = 0.7, strata = "Attrition")

train_strat <- training(split_strat)
test_strat  <- testing(split_strat)

train_strat$Attrition %>% table() %>% prop.table()
test_strat$Attrition %>% table() %>% prop.table()


# Defining formula interfaces ---------------------------------------------
# Assume model_fn() is an arbitrary function
lm(Sale_Price ~ Neighborhood + Year_Sold, data = ames)

# With interactions
lm(Sale_Price ~ Neighborhood + Year_Sold + Neighborhood:Year_Sold, data = ames)

# With transformations -- ns is natural spline fr9m splines
lm(log10(Sale_Price) ~ ns(Longitude, df = 3) + ns(Latitude, df = 3), data = ames)


# To get a flexible interface for defining functions we can use the following
model_fn(
  x = c("Year_Sold", "Longitude", "Latitude"),
  y = "Sale_Price",
  data = ames.h2o
)

# Implement K-fold validation with h2o ----
h2o.cv <- h2o.glm(
  x = x, 
  y = y,
  training_frame = ames.h2o,
  nfolds = 10 # perform 10 folds
)

vfold_cv(ames, v = 10) # when applying to an external algorithm

# Bootstrapping -----
bootstraps(ames, times = 10)

# Putting it all together -----

set.seed(123)
split <- initial_split(ames, prop = 0.7, strata = "Sale_Price")
ames_train <- training(split)
ames_test  <- testing(split)

# define resampling, grid search, and model training and validation
# Specify resampling method
cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)

# Create grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

# Tune knn model using grid
knn_fit <- train(
  Sale_Price ~ .,
  data = ames_train,
  method = "knn",
  trControl = cv,
  tuneGrid = hyper_grid,
  metric = "RMSE"
)

