# Loading libraries
library(tidyverse)
library(caret)
library(glmnet)
library(randomForest)
library(ggplot2)

library(janitor)

data <- read.csv("/Users/srihari/Desktop/US_comparision_data1.csv") %>%
  select(-X, -Year,-GDP_Billions)


# Mean Imputation for NA values
for (col in names(data)) {
  if (any(is.na(data[[col]]))) {
    data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
  }
}

# Normalizing
preProc <- preProcess(data, method = c("center", "scale"))
df <- predict(preProc, data)

#  Defining the target and features
target <- "Homeless_PerCapita"
features <- setdiff(colnames(df), target)

#  Preparing the  matrices for glmnet models
x <- model.matrix(Homeless_PerCapita ~ ., data = df)[, -1]
y <- df$Homeless_PerCapita

#  Ridge Regression with LOOCV
ridge_model <- cv.glmnet(x, y, alpha = 0, nfolds = nrow(x))
ridge_pred <- as.numeric(predict(ridge_model, newx = x, s = "lambda.min"))
ridge_metrics <- postResample(pred = ridge_pred, obs = y)

#  LASSO Regression with LOOCV
lasso_model <- cv.glmnet(x, y, alpha = 1, nfolds = nrow(x))
lasso_pred <- as.numeric(predict(lasso_model, newx = x, s = "lambda.min"))
lasso_metrics <- postResample(pred = lasso_pred, obs = y)

#  Random Forest with LOOCV 
ctrl <- trainControl(method = "LOOCV")
rf_model <- train(Homeless_PerCapita ~ ., data = df, method = "rf", trControl = ctrl, importance = TRUE)
rf_pred <- rf_model$pred$pred
rf_obs <- rf_model$pred$obs
rf_metrics <- postResample(pred = rf_pred, obs = rf_obs)

# Results

cat(" Random Forest:\n"); print(rf_metrics)
cat("\n Ridge Regression:\n"); print(ridge_metrics)
cat("\n  LASSO Regression:\n"); print(lasso_metrics)

# Feature Importance for Random Forest
rf_imp <- varImp(rf_model)$importance %>%
  rownames_to_column("Feature")

ggplot(rf_imp, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_col(fill = "red") + coord_flip() +
  labs(title = "Random Forest Feature Importance",
       x = "Feature", y = "Importance") +
  theme_minimal()

# Feature Coefficients for  LASSO
lasso_coef <- coef(lasso_model, s = "lambda.min")
lasso_df <- data.frame(
  Feature = rownames(lasso_coef),
  Coefficient = as.numeric(lasso_coef)
) %>%
  filter(Feature != "(Intercept)" & Coefficient != 0)

ggplot(lasso_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_col(fill = "purple") + coord_flip() +
  labs(title = "LASSO Feature Coefficients",
       x = "Feature", y = "Coefficient") +
  theme_minimal()

# Results

metrics_df <- tibble(
  Model = c("Random Forest", "Ridge", "LASSO"),
  RMSE = c(rf_metrics["RMSE"], ridge_metrics["RMSE"], lasso_metrics["RMSE"]),
  Rsquared = c(rf_metrics["Rsquared"], ridge_metrics["Rsquared"], lasso_metrics["Rsquared"]),
  MAE = c(rf_metrics["MAE"], ridge_metrics["MAE"], lasso_metrics["MAE"])
)

print(metrics_df)
