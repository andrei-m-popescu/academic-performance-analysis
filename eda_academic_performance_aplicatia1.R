library(tidyverse)
library(psych)
library(corrplot)

rm(list = ls())

data <- read.csv("Student_Performance.csv")

head(data)
str(data)
summary(data)

data <- data %>%
  select(
    overall_score,
    study_hours,
    attendance_percentage,
    parent_education,
    internet_access,
    school_type,
    gender,
    extra_activities
  )

str(data)

data$internet_access <- ifelse(data$internet_access == "yes", 1, 0)
data$school_type <- ifelse(data$school_type == "private", 1, 0)
data$gender <- ifelse(data$gender == "female", 1, 0)
data$extra_activities <- ifelse(data$extra_activities == "yes", 1, 0)

data$parent_education <- as.factor(data$parent_education)

summary(data)

model_simple <- lm(overall_score ~ study_hours, data = data)
summary_model <- summary(model_simple)
print(summary_model)

coeficienti <- coef(summary(model_simple))
beta0 <- coeficienti["(Intercept)", "Estimate"]
beta1 <- coeficienti["study_hours", "Estimate"]
se0 <- coeficienti["(Intercept)", "Std. Error"]
se1 <- coeficienti["study_hours", "Std. Error"]

t_beta0 <- beta0 / se0
t_beta1 <- beta1 / se1

df <- df.residual(model_simple)
t_critic <- qt(0.975, df)

cat("t calculat pentru Intercept:", t_beta0, "\n")
cat("t calculat pentru study_hours:", t_beta1, "\n")
cat("t critic la nivelul de semnificație 0.05:", t_critic, "\n")

library(tseries)

hist(resid(model_simple),
     main = "Distribuția reziduurilor",
     xlab = "Reziduuri",
     col = "lightgreen",
     border = "white")
jb_test <- jarque.bera.test(resid(model_simple))
jb_test

if(jb_test$p.value > 0.05){
  cat("Reziduurile sunt normal distribuite (p-value =", jb_test$p.value, ")\n")
} else {
  cat("Reziduurile nu sunt normal distribuite (p-value =", jb_test$p.value, ")\n")
}

library(lmtest)

white_test <- bptest(model_simple, ~ fitted(model_simple) + I(fitted(model_simple)^2))
white_test

bptest(
  model_simple,
  ~ fitted(model_simple) + I(fitted(model_simple)^2)
)

plot(data$study_hours, resid(model_simple),
     main = "Reziduuri vs. ore de studiu",
     xlab = "Ore de studiu",
     ylab = "Reziduuri",
     pch = 19,
     col = rgb(0, 0, 0, 0.4))

abline(h = 0, col = "red")

dwtest(model_simple)

new_data <- data.frame(
  study_hours = c(2, 4, 6, 8)
)

prognoze <- predict(
  model_simple,
  newdata = new_data,
  interval = "confidence",
  level = 0.90
)

prognoze


# 1) Train/Test + metrici out-of-sample
set.seed(123)
idx <- sample(seq_len(nrow(data)), size = 0.8*nrow(data))
train <- data[idx, ]
test  <- data[-idx, ]

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
mape <- function(y, yhat) mean(abs((y - yhat) / pmax(abs(y), 1e-8))) * 100
r2_test <- function(y, yhat) {
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
}



model_simple_tr <- lm(overall_score ~ study_hours, data = train)
pred_simple <- predict(model_simple_tr, newdata = test)

cat("\nMetrici out-of-sample (model simplu):\n")
cat("RMSE:", rmse(test$overall_score, pred_simple), "\n")
cat("MAE :", mae(test$overall_score, pred_simple), "\n")
cat("MAPE:", mape(test$overall_score, pred_simple), "%\n")
cat("R2  :", r2_test(test$overall_score, pred_simple), "\n")
cat("Adj R2 (train):", summary(model_simple_tr)$adj.r.squared, "\n")


# 2) Regresie multipla + evaluare out-of-sample
model_multi_tr <- lm(overall_score ~ study_hours + attendance_percentage +
                       internet_access + school_type + gender + extra_activities +
                       parent_education,
                     data = train)

summary(model_multi_tr)

pred_multi <- predict(model_multi_tr, newdata = test)

cat("\nMetrici out-of-sample (model multiplu):\n")
cat("RMSE:", rmse(test$overall_score, pred_multi), "\n")
cat("MAE :", mae(test$overall_score, pred_multi), "\n")
cat("MAPE:", mape(test$overall_score, pred_multi), "%\n")
cat("R2  :", r2_test(test$overall_score, pred_multi), "\n")
cat("Adj R2 (train):", summary(model_multi_tr)$adj.r.squared, "\n")


# 3) VIF (multicoliniaritate)
library(car)
vif(model_multi_tr)


# 4) Erori robuste (corectie pentru heteroscedasticitate)
library(sandwich)
coeftest(model_simple_tr, vcov = vcovHC(model_simple_tr, type = "HC1"))
coeftest(model_multi_tr,  vcov = vcovHC(model_multi_tr,  type = "HC1"))


# 5) Extindere: forma polinomiala + comparație
model_poly_tr <- lm(overall_score ~ study_hours + I(study_hours^2) +
                      attendance_percentage + internet_access + school_type +
                      gender + extra_activities + parent_education,
                    data = train)

summary(model_poly_tr)
anova(model_multi_tr, model_poly_tr)
AIC(model_multi_tr, model_poly_tr)
BIC(model_multi_tr, model_poly_tr)


pred_poly <- predict(model_poly_tr, newdata = test)

cat("\nMetrici out-of-sample (model polinomial):\n")
cat("RMSE:", rmse(test$overall_score, pred_poly), "\n")
cat("MAE :", mae(test$overall_score, pred_poly), "\n")
cat("MAPE:", mape(test$overall_score, pred_poly), "%\n")
cat("R2  :", r2_test(test$overall_score, pred_poly), "\n")
cat("Adj R2 (train):", summary(model_poly_tr)$adj.r.squared, "\n")



# 6) Regularizare ML: Ridge / LASSO / Elastic Net + comparatie
library(glmnet)

x_train <- model.matrix(overall_score ~ study_hours + attendance_percentage +
                          internet_access + school_type + gender + extra_activities +
                          parent_education, data = train)[,-1]
y_train <- train$overall_score

x_test <- model.matrix(overall_score ~ study_hours + attendance_percentage +
                         internet_access + school_type + gender + extra_activities +
                         parent_education, data = test)[,-1]

cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
cv_enet  <- cv.glmnet(x_train, y_train, alpha = 0.5)

pred_ridge <- as.numeric(predict(cv_ridge, s = "lambda.min", newx = x_test))
pred_lasso <- as.numeric(predict(cv_lasso, s = "lambda.min", newx = x_test))
pred_enet  <- as.numeric(predict(cv_enet,  s = "lambda.min", newx = x_test))

cat("\nComparatie RMSE (OLS vs ML):\n")
cat("OLS multiplu RMSE:", rmse(test$overall_score, pred_multi), "\n")
cat("Ridge       RMSE:", rmse(test$overall_score, pred_ridge), "\n")
cat("LASSO       RMSE:", rmse(test$overall_score, pred_lasso), "\n")
cat("Elastic Net RMSE:", rmse(test$overall_score, pred_enet), "\n")

cat("\nCoeficienți LASSO (lambda.min):\n")
coef(cv_lasso, s = "lambda.min")
