library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

getwd()
setwd("/home/carolps/IAA")

load("Bases de Dados Usadas nas Aulas Práticas-Estatistica2/trabalhosalarios.RData")

dat <- trabalhosalarios
#View(dat)

set.seed(42)
index = sample(1:nrow(dat), 0.8*nrow(dat))
train = dat[index,]
test = dat[-index,]

#dim(train)
#dim(test)

#################################### PREPROCESSAMENTO ######################################

cols = c('husage', 'husearns', 'huseduc', 'hushrs', 'earns', 'age', 'educ', 'exper', 'lwage')
pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))

train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

#summary(train)
#summary(test)

cols_reg = c('husage', 'husunion', 'husearns', 'huseduc', 'husblck', 'hushisp', 'hushrs', 'kidge6', 'age', 'black', 'educ', 'hispanic', 'union', 'exper', 'kidlt6', 'lwage')

#dat[,cols_reg]

dummies <- dummyVars(lwage~husage+husunion+husearns+huseduc+husblck+hushisp+hushrs+kidge6+age+black+educ+hispanic+union+exper+kidlt6, data = dat[,cols_reg])
train_dummies = predict(dummies, newdata = train[,cols_reg])
test_dummies = predict(dummies, newdata = test[,cols_reg])

#print(dim(train_dummies))
#print(dim(test_dummies))

x_train = as.matrix(train_dummies)
y_train = train$lwage

x_test = as.matrix(test_dummies)
y_test = test$lwage

######################################### RIDGE ############################################

lambdas <- 10^seq(5, -5, by = -1)
ridge_lamb <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas, nfolds = 10)

best_lambda_ridge <- ridge_lamb$lambda.min
best_lambda_ridge

ridge_reg = glmnet(x_train, y_train, nlambda = 50, alpha = 0, family = 'gaussian', lambda = best_lambda_ridge)
ridge_reg

# Resultado
# (coeficientes)
ridge_reg[["beta"]]

predictions_train_ridge <- predict(ridge_reg, s = best_lambda_ridge, newx = x_train)

######################################### LASSO ############################################

lambdas <- 10^seq(5, -5, by = -.1)

lasso_lamb <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 10)

best_lambda_lasso <- lasso_lamb$lambda.min 
best_lambda_lasso

lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda_lasso, standardize = TRUE)
lasso_model[["beta"]]

predictions_train_lasso <- predict(lasso_model, s = best_lambda_lasso, newx = x_train)

######################################## ELASTICNET ########################################

train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = FALSE)

elastic_reg <- train(lwage~husage+husunion+husearns+huseduc+husblck+hushisp+hushrs+
                       kidge6+age+black+educ+hispanic+union+exper+kidlt6,
                     data = train,
                     method = "glmnet",
                     tuneLength = 20,
                     trControl = train_cont)

# O melhor parametro alpha escolhido eh:
elastic_reg$bestTune

# E os parametros sao:
#elastic_reg[["finalModel"]][["beta"]]

predictions_train_elasticnet <- predict(elastic_reg, x_train)


################################### AVALIACAO DOS MODELOS ##############################

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # As metricas de performace do modelo:
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

eval_with_params <- function(model, best_lambda = NULL) {
  
  husage = (40-pre_proc_val[["mean"]][["husage"]])/pre_proc_val[["std"]][["husage"]]
  husunion = 0
  husearns = (600-pre_proc_val[["mean"]][["husearns"]])/pre_proc_val[["std"]][["husearns"]]
  huseduc = (13-pre_proc_val[["mean"]][["huseduc"]])/pre_proc_val[["std"]][["huseduc"]]
  husblck = 1
  hushisp = 0
  hushrs = (40-pre_proc_val[["mean"]][["hushrs"]])/pre_proc_val[["std"]][["hushrs"]]
  kidge6 = 1
  age = (38-pre_proc_val[["mean"]][["age"]])/pre_proc_val[["std"]][["age"]]
  black = 0
  educ = (13-pre_proc_val[["mean"]][["educ"]])/pre_proc_val[["std"]][["educ"]]
  hispanic = 1
  union = 0
  exper = (18-pre_proc_val[["mean"]][["exper"]])/pre_proc_val[["std"]][["exper"]]
  kidlt6 = 1
  
  our_pred = as.matrix(data.frame(husage=husage, 
                                  husunion=husunion,
                                  husearns=husearns,
                                  huseduc=huseduc,
                                  husblck=husblck,
                                  hushisp=hushisp,
                                  hushrs=hushrs,
                                  kidge6=kidge6,
                                  age=age,
                                  black=black,
                                  educ=educ,
                                  hispanic=hispanic,
                                  union=union,
                                  exper=exper,
                                  kidlt6=kidlt6))
  
  prediction <- predict(model, s = best_lambda, newx = our_pred)
  
  if (is.null(best_lambda)) {
    prediction <- predict(model, our_pred)
  }
  
  wage_pred=(prediction*pre_proc_val[["std"]][["lwage"]])+pre_proc_val[["mean"]][["lwage"]]
  
  cat("Predicao: ", wage_pred, "\n")
  cat("Predicao exp: ", exp(wage_pred), "\n")
  
  return(wage_pred)
}

calculate_intervals <- function(wage_pred) {
  
  # O intervalo de confianca
  n <- nrow(train)
  m <- wage_pred
  s <- pre_proc_val[["std"]][["lwage"]]
  dam <- s/sqrt(n)
  CIlwr <- m + (qnorm(0.025))*dam # intervalo inferior
  CIupr <- m - (qnorm(0.025))*dam # intervalo superior
  
  cat("Intervalo inferior: ", CIlwr, "\n")
  cat("Intervalo superior: ", CIupr, "\n")
  
  cat("Intervalo inferior exp: ", exp(CIlwr), "\n")
  cat("Intervalo superior exp: ", exp(CIupr), "\n")
}

# -------- RIDGE -------- #

eval_results(y_train, predictions_train_ridge, train)

predictions_test <- predict(ridge_reg, s = best_lambda_ridge, newx = x_test)
eval_results(y_test, predictions_test, test)

predict_our_ridge <- eval_with_params(ridge_reg, best_lambda = best_lambda_ridge)

calculate_intervals(predict_our_ridge)

# -------- LASSO -------- #

eval_results(y_train, predictions_train_lasso, train)

predictions_test <- predict(lasso_model, s = best_lambda_lasso, newx = x_test)
eval_results(y_test, predictions_test, test)

predict_our_lasso <- eval_with_params(lasso_model, best_lambda = best_lambda_lasso)

calculate_intervals(predict_our_lasso)

# ------- ELATICNET ----- #

eval_results(y_train, predictions_train_elasticnet, train) 
predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)

predict_our_elastic <- eval_with_params(elastic_reg)

calculate_intervals(predict_our_elastic)
