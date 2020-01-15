# Logistic regression for prediction ----

pacman::p_load("tidyverse", "caret", "vip", "patchwork", "rsample", "RColorBrewer")


# Focus on the attrition data

df <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

# Create training (70%) and test (30%) sets for the 
# rsample::attrition data.
set.seed(123)  # for reproducibility
churn_split <- initial_split(df, prop = .7, strata = "Attrition")
churn_train <- training(churn_split)
churn_test  <- testing(churn_split)

model1 <- glm(Attrition ~ MonthlyIncome, family = "binomial", data = churn_train)
model2 <- glm(Attrition ~ OverTime, family = "binomial", data = churn_train)

m_list <- list(model1, model2)
purrr::map(m_list, broom::tidy)


confint(model1)
# For odds ratios
exp(confint(model1))


purrr::map(m_list, coef %>% exp)

# Showing maximum likelihood results ----
churn_train2 <- churn_train %>% mutate(prob = ifelse(Attrition == "Yes", 1, 0))
churn_train2 <- broom::augment(model2, churn_train2) %>% mutate(.fitted = exp(.fitted))

p1 <- ggplot(churn_train2, aes(MonthlyIncome, prob)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Predicted probabilities for model1") +
  xlab("Monthly Income") +
  ylab("Probability of Attrition")

p2 <- ggplot(churn_train2, aes(OverTime, .fitted, color = OverTime)) +
  geom_boxplot(show.legend = FALSE) +
  geom_rug(sides = "b", position = "jitter", alpha = 0.2, show.legend = FALSE) +
  ggtitle("Predicted probabilities for model2") +
  xlab("Over Time") +
  scale_y_continuous("Probability of Attrition", limits = c(0, 1))
p1 + p2 + theme_minimal()

# Multiple logistic regression ----
model3 <- glm(
  Attrition ~ MonthlyIncome + OverTime,
  family = "binomial", 
  data = churn_train
)

tidy(model3)

churn_train3 <- churn_train %>% mutate(prob = ifelse(Attrition == "Yes", 1, 0))
churn_train3 <- broom::augment(model3, churn_train3) %>% mutate(.fitted = exp(.fitted))

ggplot(churn_train3, aes(MonthlyIncome, prob, color = OverTime)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  ggtitle("Predicted probabilities for model3") +
  scale_colour_brewer(palette  = "Accent") +
  theme_minimal() +
  xlab("Monthly Income") +
  ylab("Probability of Attrition")

# Train and test 3 models -----
set.seed(123)
cv_model1 <- train(
  Attrition ~ MonthlyIncome, 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model2 <- train(
  Attrition ~ MonthlyIncome + OverTime, 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model3 <- train(
  Attrition ~ ., 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# extract out of sample performance measures
summary(
  resamples(
    list(
      model1 = cv_model1, 
      model2 = cv_model2, 
      model3 = cv_model3
    )
  )
)$statistics$Accuracy

# Confusion matrix ----
pred_class <- predict(cv_model3, churn_train)

# create confusion matrix
confusionMatrix(
  data = relevel(pred_class, ref = "Yes"), 
  reference = relevel(churn_train$Attrition, ref = "Yes")
  
# ROC curve ----
library(ROCR)

# Compute predicted probabilities
m1_prob <- predict(cv_model1, churn_train, type = "prob")$Yes
m3_prob <- predict(cv_model3, churn_train, type = "prob")$Yes

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, churn_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")
perf2 <- prediction(m3_prob, churn_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = "black", lty = 2)
plot(perf2, add = TRUE, col = "blue")
legend(0.8, 0.2, legend = c("cv_model1", "cv_model3"),
       col = c("black", "blue"), lty = 2:1, cex = 0.6)
)

vip(cv_model3, num_features = 20)
