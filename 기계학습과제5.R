install.packages("forecastML")
install.packages("glmnet")
install.packages("randomForest")
install.packages('e1071')
install.packages('adabag')
install.packages("xgboost")
library(forecastML)
library(dplyr)
library(DT)
library(ggplot2)
library(glmnet)
library(randomForest)
library(e1071)
library(adabag)
library(xgboost)
# 데이터 불러오기
data("data_seatbelts", package = "forecastML")
data <- data_seatbelts

date_frequency <- "1 month"  # Time step frequency.

# 날짜 sequence 생성
dates <- seq(as.Date("1969-01-01"), as.Date("1984-12-01"), by = date_frequency)


data$PetrolPrice <- round(data$PetrolPrice, 3) # 소수점 3자리까지 표현
data <- data[, c("DriversKilled", "kms", "PetrolPrice", "law")]  # 사용 할 변수 선택
DT::datatable(head(data, 5)) # 데이터 예시  


# 마지막 12달 : test data
# 12달 제외 : train data
data_train <- data[1:(nrow(data) - 12), ]
data_test <- data[(nrow(data) - 12 + 1):nrow(data), ]


# 날짜별 Driverskilled 시각화
p <- ggplot(data, aes(x = dates, y = DriversKilled)) # plot
p <- p + geom_line() # 선으로 표현
p <- p + geom_vline(xintercept = dates[nrow(data_train)], color = "red", size = 1.1) # train test 분리선
p <- p + theme_bw() + xlab("Dataset index") # 배경 밑 x축 label 변경
p

outcome_col <- 1  # 반응변수 열의 번호

# 예측 기간 종류( 1-step-ahead, 3-step-ahead , ...)
horizons <- c(1, 3, 6, 12)

# 변수로 사용할 과거 시점들
lookback <- c(1:6, 9, 12, 15)

dynamic_features <- "law" # 현재 시점에만 사용되는 변수


# horizons와 lookback의 조합으로 data_list 생성
# horizons의 개수만큼 list 생성
data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = date_frequency,
                                          dynamic_features = dynamic_features
)

# horizons = 6 일 때의 예시
DT::datatable(head(data_list$horizon_6, 10), options = list(scrollX = TRUE))

plot(data_list) # 각 horizons 별 사용할 시점과 예측할 미래 시점 시각화


# cross vaidation을 위한 기간 분할
# window_length = 24  ==> 한 block 당 데이터 수 = 24 
windows <- forecastML::create_windows(lagged_df = data_list, window_length = 24, skip = 0,
                                      window_start = NULL, window_stop = NULL,
                                      include_partial_window = TRUE)

# 시각화
plot(windows, data_list, show_labels = TRUE)



# Example 1 - LASSO

# 데이터를 LASSO에 적합하여 model로 return하는 함수 정의
# 이후에 train_model() 에 쓰기 위함
model_function <- function(data) {
  
  # The 'law' feature is constant during some of our outer-loop validation datasets so we'll 
  # simply drop it so that glmnet converges.
  constant_features <- which(unlist(lapply(data[, -1], function(x) {!(length(unique(x)) > 1)})))
  
  if (length(constant_features) > 1) {
    data <- data[, -c(constant_features + 1)]  # +1 because we're skipping over the outcome column.
  }
  
  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))
  
  model <- glmnet::cv.glmnet(x, y, nfolds = 3)
  return(list("model" = model, "constant_features" = constant_features))
}


# Example 2 - Random Forest
# 데이터를 RandomForest에 적합하여 model로 return하는 함수 정의
# 이후에 train_model() 에 쓰기 위함
model_function_2 <- function(data) {
  
  outcome_names <- names(data)[1] #DriversKilled
  model_formula <- formula(paste0(outcome_names,  "~ ."))
  
  model <- randomForest::randomForest(formula = model_formula, data = data, ntree = 200)
  return(model)
}

##############################################################################
# TODO: LASSO와 RF를 제외한 나머지 방법들(SVM, Bagging, Boosting, Ridge 등)로 #
# 사용자 함수를 정의하고 예측값을 비교해서 어느 방법이 가장 좋은 결과를 제공  #
# 하는지 코드와 결과를 정리해서 압축화일로 제출하세요.                        #
##############################################################################

# Example 3 - Ridge

model_function_ridge<-function(data){
  
  # The 'law' feature is constant during some of our outer-loop validation datasets so we'll 
  # simply drop it so that glmnet converges.
  constant_features <- which(unlist(lapply(data[, -1], function(x) {!(length(unique(x)) > 1)})))
  
  if (length(constant_features) > 1) {
    data <- data[, -c(constant_features + 1)]  # +1 because we're skipping over the outcome column.
  }
  
  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))
  
  model <- glmnet::cv.glmnet(x, y,alpha = 1, nfolds = 3)
  return(list("model" = model, "constant_features" = constant_features))
}

# Example 4 - ElasticNet

model_function_Elas<-function(data){
  
  # The 'law' feature is constant during some of our outer-loop validation datasets so we'll 
  # simply drop it so that glmnet converges.
  constant_features <- which(unlist(lapply(data[, -1], function(x) {!(length(unique(x)) > 1)})))
  
  if (length(constant_features) > 1) {
    data <- data[, -c(constant_features + 1)]  # +1 because we're skipping over the outcome column.
  }
  
  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))
  
  model <- glmnet::cv.glmnet(x, y,alpha = 0.5, nfolds = 3)
  return(list("model" = model, "constant_features" = constant_features))
}
#Example 5 - svm
model_function_svm<-function(data){
  
  outcome_names <- names(data)[1] #DriversKilled
  model_formula <- formula(paste0(outcome_names,  "~ ."))
  model <- svm(formula=model_formula,data=data)
  return(model)
}
#Example 6 - xgboost
model_function_xgb<-function(data){
  constant_features <- which(unlist(lapply(data[, -1], function(x) {!(length(unique(x)) > 1)})))
  
  if (length(constant_features) > 1) {
    data <- data[, -c(constant_features + 1)]  # +1 because we're skipping over the outcome column.
  }
  
  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))
  xgb_train <- xgb.DMatrix(data = x, label = y)
  xgb_params <- list(
    booster = "gbtree",
    eta = 0.01,
    max_depth = 8,
    gamma = 4,
    subsample = 0.75,
    colsample_bytree = 1,
    objective = "multi:softprob",
    eval_metric = "mlogloss",
  )
  model <- xgb.cv(
    data=xgb_train,nfold=10, nrounds=nround.cv, prediction=TRUE, verbose=0
  )
  return(model)
}
#Example 7 - bagging
model_function_bag<-function(data){
  
  outcome_names <- names(data)[1] #DriversKilled
  model_formula <- formula(paste0(outcome_names,  "~ ."))
  model <- bagging.cv(formula=model_formula,data=data)
  return(model)
}


# LASSO 모형 cross_validation을 이용하여 훈련
model_results <- forecastML::train_model(data_list, windows, model_name = "LASSO",
                                         model_function, use_future = FALSE)

# RandomForest 모형 cross_validation을 이용하여 훈련
model_results_2 <- forecastML::train_model(data_list, windows, model_name = "RF", 
                                           model_function_2, use_future = FALSE)
# Ridge 모형 cross_validation을 이용하여 훈련
model_results_3 <- forecastML::train_model(data_list, windows, model_name = "RI", 
                                           model_function_ridge, use_future = FALSE)
# Elasticnet 모형 cross_validation을 이용하여 훈련
model_results_4 <- forecastML::train_model(data_list, windows, model_name = "EL", 
                                           model_function_Elas, use_future = FALSE)
# svm 모형 cross_validation을 이용하여 훈련
model_results_5 <- forecastML::train_model(data_list, windows, model_name = "SVM", 
                                           model_function_svm, use_future = FALSE)
# xgb 모형 cross_validation을 이용하여 훈련
#model_results_6 <- forecastML::train_model(data_list, windows, model_name = "xgb", 
                                           #model_function_xgb, use_future = FALSE)
# bagging 모형 cross_validation을 이용하여 훈련
#model_results_7 <- forecastML::train_model(data_list, windows, model_name = "BAG", 
                                           #model_function_bag, use_future = FALSE)
# Example 1 - LASSO
# 훈련된 LASSO 모델을 이용해 predict하여 data.frame으로 return 하는 함수 정의 
prediction_function <- function(model, data_features) {
  
  if (length(model$constant_features) > 1) {  # 'model' was passed as a list.
    data <- data[, -c(model$constant_features )]
  }
  
  x <- as.matrix(data_features, ncol = ncol(data_features))
  
  data_pred <- data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}

# Example 2 - Random Forest
# 훈련된 RF 모델을 이용해 predict하여 data.frame으로 return 하는 함수 정의
prediction_function_2 <- function(model, data_features) {
  
  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}
# Example 3 - Ridge
# 훈련된 Ridge 모델을 이용해 predict하여 data.frame으로 return 하는 함수 정의
prediction_function_3 <- function(model, data_features){
  
  if (length(model$constant_features) > 1) {  # 'model' was passed as a list.
    data <- data[, -c(model$constant_features )]
  }
  
  x <- as.matrix(data_features, ncol = ncol(data_features))
  
  data_pred <- data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}
# Example 4 - ElasticNet
# 훈련된 ElasticNet 모델을 이용해 predict하여 data.frame으로 return 하는 함수 정의
prediction_function_4 <- function(model, data_features){
  
  if (length(model$constant_features) > 1) {  # 'model' was passed as a list.
    data <- data[, -c(model$constant_features )]
  }
  
  x <- as.matrix(data_features, ncol = ncol(data_features))
  
  data_pred <- data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}
# Example 5 - SVM
# 훈련된 SVM 모델을 이용해 predict하여 data.frame으로 return 하는 함수 정의
prediction_function_5 <- function(model, data_features) {
  
  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}
# Example 6 - boosting
# 훈련된 boosting 모델을 이용해 predict하여 data.frame으로 return 하는 함수 정의
prediction_function_6 <- function(model, data_features) {
  
  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}
# Example 7 - bagging
# 훈련된 bagging 모델을 이용해 predict하여 data.frame으로 return 하는 함수 정의
prediction_function_7 <- function(model, data_features) {
  
  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}
# 각 모델과 horizons별 예측 결과 
# windows 별 결과도 보여줌
data_results <- predict(model_results, model_results_2,model_results_3,model_results_4,model_results_5,
                        prediction_function = list(prediction_function, prediction_function_2,prediction_function_3,prediction_function_4,prediction_function_5), 
                        data = data_list)

# 예측값 정수로 만듦
data_results$DriversKilled_pred <- round(data_results$DriversKilled_pred, 0)

# 예측값 보기
DT::datatable(head(data_results, 30), options = list(scrollX = TRUE))


# horizons 별 결과 시각화 ( 예측값, 잔차, 안정성 )
plot(data_results, type = "prediction", horizons = c(1, 6, 12))
plot(data_results, type = "residual", horizons = c(1, 6, 12), windows = 5:7)
plot(data_results, type = "forecast_stability", windows = max(data_results$window_number))

# 예측결과 지표들을 보여줌 (mae, mape, smape, 등등..)
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "smape"))
data_error$error_global[, c("mae", "mape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "smape")], round, 1)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))

# 예측오차의 다양한 시각화
plot(data_error, data_results, type = "time", facet = ~ horizon, horizons = c(1, 6, 12,18,24), windows = 5:7)
plot(data_error, data_results, type = "horizon", facet = ~ horizon, horizons = c(1, 6, 12,18,24))
plot(data_error, data_results, type = "global")


# hyperparameters를 살펴보기 위해 model의 hyperparameter를 return 하는 함수 정의
hyper_function <- function(model) {
  
  lambda_min <- model$model$lambda.min
  lambda_1se <- model$model$lambda.1se
  
  data_hyper <- data.frame("lambda_min" = lambda_min, "lambda_1se" = lambda_1se)
  return(data_hyper)
}

# return_hyper를 통해 hyperparameter 살펴보기
data_hyper <- forecastML::return_hyper(model_results, hyper_function)

# hyperparameter별 결과 시각화
plot(data_hyper, data_results, data_error, type = "stability", horizons = c(1, 6, 12))
plot(data_hyper, data_results, data_error, type = "error", c(1, 6, 12))


# test data에 대한 예측을 위한 forecast dataset 생성
# type = 'forecast'
data_forecast_list <- forecastML::create_lagged_df(data_train,
                                                   outcome_col = outcome_col,
                                                   type = "forecast",
                                                   horizons = horizons,
                                                   lookback = lookback,
                                                   date = dates[1:nrow(data_train)],
                                                   frequency = date_frequency,
                                                   dynamic_features = dynamic_features
)

# forecast dataset 예시
DT::datatable(head(data_forecast_list$horizon_6), options = list(scrollX = TRUE))


# 안전벨트 법 도입시기 이므로 law = 1 대입
for (i in seq_along(data_forecast_list)) {
  data_forecast_list[[i]]$law <- 1
}

# forecast dataset에 대해 훈련된 모델 적용
data_forecast <- predict(model_results, model_results_2, model_results_3,model_results_4,model_results_5,
                         prediction_function = list(prediction_function, prediction_function_2,prediction_function_3,prediction_function_4,prediction_function_5), 
                         data = data_forecast_list)


# horizons 마다의 forecast 결과
data_forecast$DriversKilled_pred <- round(data_forecast$DriversKilled_pred, 0)
DT::datatable(head(data_forecast, 10), options = list(scrollX = TRUE))


# forecast dataset의 예측결과 시각화
# 각 windows별 훈련된 모델로 예측하기 때문에 plot마다 7개의 예측결과를 나타냄
plot(data_forecast,
     data_actual = data[-(1:150), ],  # Actuals from the training and test data sets.
     actual_indices = dates[-(1:150)], 
     horizons = c(1, 6, 12))

# forecast error 보기
data_error <- forecastML::return_error(data_forecast,
                                       data_test = data_test,
                                       test_indices = dates[(nrow(data_train) + 1):length(dates)],
                                       metrics = c("mae", "mape", "smape", "mdape"))

data_error$error_by_horizon[, c("mae", "mape", "smape", "mdape")] <- lapply(data_error$error_by_horizon[, c("mae", "mape", "smape", "mdape")], round, 1)
DT::datatable(head(data_error$error_by_horizon, 10), options = list(scrollX = TRUE))  # LASSO로 결정


# training dataset을 전부 훈련하기 대한 data list 생성
data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = date_frequency,
                                          dynamic_features = dynamic_features
)

# training dataset 전부 훈련해야하므로 window_length=0 으로 지정
windows <- forecastML::create_windows(data_list, window_length = 0)
plot(windows, data_list, show_labels = TRUE) # 시각화

# training datasest 학습
# LASSO로 결정
model_results <- forecastML::train_model(data_list, windows,  model_name = "SVM", model_function_svm)

# historical data 예측 (training dataset을 예측)
data_results <- predict(model_results, prediction_function = list(prediction_function_5), data = data_list)
DT::datatable(head(data_results, 10), options = list(scrollX = TRUE))

plot(data_results, type = "prediction", horizons = c(1, 6, 12)) # horizons 에 따른 시각화

# 예측 결과 지표 return
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "mdape", "smape"),
                                       models = NULL)

data_error$error_global[, c("mae", "mape", "mdape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "mdape", "smape")], round, 1)
DT::datatable(head(data_error$error_global), options = list(scrollX = TRUE))



# forecast data list 생성
data_forecast_list <- forecastML::create_lagged_df(data_train,
                                                   outcome_col = outcome_col,
                                                   type = "forecast",
                                                   horizons = horizons,
                                                   lookback = lookback,
                                                   date = dates[1:nrow(data_train)],
                                                   frequency = date_frequency,
                                                   dynamic_features = dynamic_features
)

# 안전벨트 법 도입 (law = 1)
for (i in seq_along(data_forecast_list)) {
  data_forecast_list[[i]]$law <- 1
}

# training dataset 전체로 훈련된 모델을 forecast data에 적용
data_forecast <- predict(model_results, prediction_function = list(prediction_function), data = data_forecast_list)

# 예측 결과 시각화
plot(data_forecast,
     data_actual = data[-(1:150), ],
     actual_indices = dates[-(1:150)])


# 예측 결과 지표 return
data_error <- forecastML::return_error(data_forecast, data_test = data_test, 
                                       test_indices = dates[(nrow(data_train) + 1):nrow(data)],
                                       metrics = c("mae", "mape", "mdape", "smape"))

data_error$error_by_horizon[, c("mae", "mape", "mdape", "smape")] <- lapply(data_error$error_by_horizon[, c("mae", "mape", "mdape", "smape")], round, 1)
data_error$error_global[, c("mae", "mape", "mdape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "mdape", "smape")], round, 1)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))


# 각 h-step-ahead forecasting을 결합
data_combined <- forecastML::combine_forecasts(data_forecast)


data_actual <- data[dates >= as.Date("1980-01-01"), ]
actual_indices <- dates[dates >= as.Date("1980-01-01")]

# 모델 결합 결과 시작화
plot(data_combined, data_actual = data_actual, actual_indices = actual_indices)
