# Linear regression for prediction ----

pacman::p_load("tidyverse", "caret", "vip", "patchwork")


# Stratified sampling with the rsample package
set.seed(123)
split <- initial_split(ames, prop = 0.7, 
                       strata = "Sale_Price")
ames_train  <- training(split)
ames_test   <- testing(split)


# Train a simple model ----
model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames_train)
summary(model1)

# Plots for the model
p1 <- model1 %>%
  broom::augment() %>%  # This converts model results to a usable dataframe %>%
  ggplot(aes(Gr_Liv_Area, Sale_Price)) + 
  geom_point(size = 1, alpha = 0.3) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Fitted regression line") +
  theme_minimal()

# Fitted regression line (restricted range)
p2 <- model1 %>%
  broom::augment() %>%
  ggplot(aes(Gr_Liv_Area, Sale_Price)) + 
  geom_segment(aes(x = Gr_Liv_Area, y = Sale_Price,
                   xend = Gr_Liv_Area, yend = .fitted), 
               alpha = 0.3) +
  geom_point(size = 1, alpha = 0.3) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Fitted regression line (with residuals)") +
  theme_minimal()
p1 + p2

# Confidence interval at diffent widths
confint(model1, level = 0.95)
confint(model1, level = 0.90) 

# Extend model to make it multivariate ----
(model2 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames_train))

# With update
(model2 <- update(model1, . ~ . + Year_Built))

# With interactions
lm(Sale_Price ~ Gr_Liv_Area + Year_Built + Gr_Liv_Area:Year_Built, data = ames_train)

# Contour plots of interactions ----
# Fitted models
fit1 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames_train)
fit2 <- lm(Sale_Price ~ Gr_Liv_Area * Year_Built, data = ames_train)

# Regression plane data
plot_grid <- expand.grid(
  Gr_Liv_Area = seq(from = min(ames_train$Gr_Liv_Area), to = max(ames_train$Gr_Liv_Area), 
                    length = 100), 
  Year_Built = seq(from = min(ames_train$Year_Built), to = max(ames_train$Year_Built), 
                   length = 100)
)
plot_grid$y1 <- predict(fit1, newdata = plot_grid)
plot_grid$y2 <- predict(fit2, newdata = plot_grid)

# Level plots
p1 <- ggplot(plot_grid, aes(x = Gr_Liv_Area, y = Year_Built, 
                            z = y1, fill = y1)) +
  geom_tile() +
  geom_contour(color = "white") +
  viridis::scale_fill_viridis(name = "Predicted\nvalue", option = "inferno") +
  theme_bw() +
  ggtitle("Main effects only")
p2 <- ggplot(plot_grid, aes(x = Gr_Liv_Area, y = Year_Built, 
                            z = y2, fill = y1)) +
  geom_tile() +
  geom_contour(color = "white") +
  viridis::scale_fill_viridis(name = "Predicted\nvalue", option = "inferno") +
  theme_bw() +
  ggtitle("Main effects with two-way interaction")

p1 + p2

# Incorporate all covariates ----
model3 <- lm(Sale_Price ~ ., data = ames_train) 
broom::tidy(model3) %>% arrange(-p.value)

# Cross validation to determine best model ----
set.seed(123)  # for reproducibility
(cv_model1 <- train(
  form = Sale_Price ~ Gr_Liv_Area, 
  data = ames_train, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))


# model 2 CV
set.seed(123)
cv_model2 <- train(
  Sale_Price ~ Gr_Liv_Area + Year_Built, 
  data = ames_train, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

# model 3 CV
set.seed(123)
cv_model3 <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

# Extract out of sample performance measures
summary(resamples(list(
  model1 = cv_model1, 
  model2 = cv_model2, 
  model3 = cv_model3
)))

# Transformation ----
p1 <- ggplot(ames_train, aes(Year_Built, Sale_Price)) + 
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE) +
  scale_y_continuous("Sale price", labels = scales::dollar) +
  xlab("Year built") +
  ggtitle(paste("Non-transformed variables with a\n",
                "non-linear relationship."))

p2 <- ggplot(ames_train, aes(Year_Built, Sale_Price)) + 
  geom_point(size = 1, alpha = .4) + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10("Sale price", labels = scales::dollar, 
                breaks = seq(0, 400000, by = 100000)) +
  xlab("Year built") +
  ggtitle(paste("Transforming variables can provide a\n",
                "near-linear relationship."))
p1 + p2


# Checking for constant error variance (heteroskedasticity or homoskedasticity?) ----
# Use the broom augment to add residuals
df1 <- broom::augment(cv_model1$finalModel, data = ames_train)

p1 <- ggplot(df1, aes(.fitted, .resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Sale_Price ~ Gr_Liv_Area")

df2 <- broom::augment(cv_model3$finalModel, data = ames_train)

p2 <- ggplot(df2, aes(.fitted, .resid)) + 
  geom_point(size = 1, alpha = .4)  +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 3", subtitle = "Sale_Price ~ .")
p1 + p2

# Autocorrelation ----
df1 <- mutate(df1, id = row_number())
df2 <- mutate(df2, id = row_number())

p1 <- ggplot(df1, aes(id, .resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Correlated residuals.")

p2 <- ggplot(df2, aes(id, .resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 3", subtitle = "Uncorrelated residuals.")

p1 + p2


# Principal components regression -----------------------------------------

set.seed(123)
cv_model_pcr <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20
)


# Partial Least Squares ----
library(AppliedPredictiveModeling)
library(recipes)
library(tidyr)

data(solubility)
df <- cbind(solTrainX, solTrainY)

pca_df <- recipe(solTrainY ~ ., data = df) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors()) %>%
  prep(training = df, retain = TRUE) %>%
  juice() %>%
  select(PC1, PC2, solTrainY) %>%
  rename(`PCR Component 1` = "PC1", `PCR Component 2` = "PC2") %>%  
  gather(component, value, -solTrainY)

pls_df <- recipe(solTrainY ~ ., data = df) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pls(all_predictors(), outcome = "solTrainY") %>%
  prep(training = df, retain = TRUE) %>%
  juice() %>%
  rename(`PLS Component 1` = "PLS1", `PLS Component 2` = "PLS2") %>%
  gather(component, value, -solTrainY)

pca_df %>% 
  bind_rows(pls_df) %>%
  ggplot(aes(value, solTrainY)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "lm", se = FALSE, lty = "dashed") +
  facet_wrap(~ component, scales = "free") +
  labs(x = "PC Eigenvalues", y = "Response")

# Fit a PLS model
set.seed(123)
cv_model_pls <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20
)

cv_model_pls$bestTune
ggplot(cv_model_pls)


# Plotting factors and relative importance ----
vip(cv_model_pls, num_features = 20, method = "model")

pdp::partial(cv_model_pls, "Gr_Liv_Area", grid.resolution = 20, plot = TRUE) 

#