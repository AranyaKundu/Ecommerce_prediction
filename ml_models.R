# load necessary libraries
library(splitstackshape)
library(forecast)
library(gam)
library(rpart)
library(tibble)
library(randomForest)
library(xgboost)
library(caret)
library(dplyr)

# load the data
load("data.rda")

# select necessary columns for machine learning models and time series analysis
mdf <- data[, -c(1:3, 5, 6, 8, 12, 14, 17)]
set.seed(1234) # set seed
# split data using stratified sampling method
split_dataset <- splitstackshape::stratified(mdf, 
                                             group = c("Category", "Market", "Ship_Mode"), 
                                             size = 0.25, bothSets = T)
test_dataset <- split_dataset[[1]]
train_dataset <- split_dataset[[2]]


fit_ml_models <- function(model){
  if (model == "Linear Regression"){
    # fit linear regression
    linear_model <- lm(Profit ~ ., data = train_dataset[, c(1:2, 6:12)])
    lm_pred <- predict(linear_model, test_dataset)
    lm_accr <- accuracy(lm_pred, test_dataset$Profit)
    res <- cbind(as.data.frame(lm_accr), t(linear_model$coefficients)) |> 
      t() |> as.data.frame()
    colnames(res)[1] <- "Model Details"
    return (res)
  }
  else if (model == "General Additive Model"){
    # fit general additive model
    formula = paste0("Profit ~ Ship_Mode + Segment + Market + Category + s(Sales, 4) + 
        s(Quantity, 4) + s(Discount, 4) + s(Shipping_Cost, 4) + Order_Priority") %>% 
      as.formula()
    gam_model <- gam(formula, data = train_dataset[, c(1:2, 6:13)])
    gam_pred <- predict(gam_model, newdata = test_dataset)
    gam_accr <- accuracy(gam_pred, test_dataset$Profit)
    res <- cbind(as.data.frame(gam_accr), t(gam_model$coefficients)) |> 
      t() |> as.data.frame()
    colnames(res)[1] <- "Model Details"
    return (res)
  }
}

subcat <- unique(data$`Sub-Category`)
ml_models_classification <- function(model, prd_subcat) {
  # fit classification models
  if(model == "Decision Tree Classifier") {
    decision_tree_fit <- rpart(Category ~ ., data = train_dataset[, -c(3:5)])
    preds_fit_test <- predict(decision_tree_fit, newdata = test_dataset, type = "class") |> as.data.frame()
    preds_fit_train <- predict(decision_tree_fit, newdata = train_dataset[, -c(3:5)], type = "class") |> 
      as.data.frame()
    names(preds_fit_test) <- "Predicted"
    names(preds_fit_train) <- "Predicted"
    cmb_df <- rbind(add_column(train_dataset[, c(3:8)], preds_fit_train),
                    add_column(test_dataset[, c(3:8)], preds_fit_test)
                    )
  }
  else if(model == "Bootstrap Aggregation") {
    # fit bootstrap aggregation/ bagging model
    bag_model_fit <- randomForest(factor(Category) ~ .,
                 data = na.omit(train_bag[, -c(3:5, 8)]),
                 ntree = 150,
                 mtry = 9)
    preds_fit_test <- predict(bag_model_fit, newdata = test_dataset[, -c(3:5, 8)], type = "class") |>
      as.data.frame()
    preds_fit_train <- predict(bag_model_fit, newdata = train_dataset[, -c(3:5, 8)], type ="class") |>
      as.data.frame()
    names(preds_fit_test) <- "Predicted"
    names(preds_fit_train) <- "Predicted"
    cmb_df <- rbind(add_column(train_dataset[, c(3:8)], preds_fit_train),
                    add_column(test_dataset[, c(3:8)], preds_fit_test)
    )
  }
  else if(model == "Xtreme Gradient Boost") {
    # fit xgboost model
    train_boost <- train_dataset[,-8]
    test_boost <- test_dataset[, -8]
    train_boost[, c("Ship_Mode", "Segment", "Category", "Market", "Order_Priority")] <- lapply(
      train_boost[, c("Ship_Mode", "Segment", "Category", "Market", "Order_Priority")], 
      function(x) as.numeric(factor(x)) - 1) # prepare categorical variables in train data set for DMatrix
    test_boost[, c("Ship_Mode", "Segment", "Category", "Market", "Order_Priority")] <- lapply(
      test_boost[, c("Ship_Mode", "Segment", "Category", "Market", "Order_Priority")], 
      function(x) as.numeric(factor(x)) - 1) # prepare categorical variables in test dataset for DMatrix
    
    # compute DMatrix for train data
    set.seed(123456)
    dtrain <- xgb.DMatrix(data = as.matrix(train_boost[, -c(3:5, 7)]),
                          label = train_boost$Category)
    # compute DMatrix for test data
    dtest <- xgb.DMatrix(data = as.matrix(test_boost[, -c(3:5, 7)]))

    
    # # fit xgboost model
    # model_fit <- xgboost(data = dtrain,
    #                      eta = 0.3, # set learning rate
    #                      max.depth = 10,
    #                      min_child_weight = 10,
    #                      subsample = 0.8,
    #                      colsample_bytree = 0.8,
    # 
    #                      verbose = 1,
    #                      nrounds = 500,
    #                      early_stopping_rounds = 20,
    #                      gamma = 0.1,
    #                      nthread = 1,
    #                      print_every_n = 20,
    #                      num_class = length(unique(train_dataset$Category)),
    #                      objective = "multi:softmax",
    #                      eval_metric = "auc",
    #                      eval_metric = "merror"
    #                      )
    saveRDS(model_fit, "xgb_model.rds") # save the model to reduce run time in shiny
    readRDS("xgb_model.rds")
    preds_fit_test <- predict(model_fit, dtest, type = "class") |> as.data.frame()
    preds_fit_train <- predict(model_fit, dtrain, type ="class") |> as.data.frame()
    names(preds_fit_test) <- "Predicted"
    names(preds_fit_train) <- "Predicted"
    preds_fit_test$Predicted <- ifelse(preds_fit_test$Predicted == 2, "Technology", 
                             ifelse(preds_fit_test$Predicted == 0, "Furniture", "Office Supplies"))
    preds_fit_train$Predicted <- ifelse(preds_fit_train$Predicted == 2, "Technology", 
                             ifelse(preds_fit_train$Predicted == 0, "Furniture", "Office Supplies"))
    cmb_df <- rbind(add_column(train_dataset[, c(3:8)], preds_fit_train),
                    add_column(test_dataset[, c(3:8)], preds_fit_test)
    )
    cfm_data <- predict(model_fit, dtest, type = "class")
    
  }
  
  cfm <- confusionMatrix(table(cfm_data, test_boost$Category), positive = "Yes")
  
  output_df <- cmb_df %>% filter(`Sub-Category` == prd_subcat)
  return (output_df)
  
}


# Time Series Models below

ts_predict <- function(selectcol, validation) {
  monthly_sales <- data %>% group_by(year_month = format(Order_Date, "%Y-%m")) %>% 
    summarise(Total_sales = sum(Sales),
              Total_Profit = sum(Profit))
  
  if (selectcol == "Profit") {
    ts_data <- ts(monthly_sales[, 3], frequency = 12, 
                    start = c(2012, 01), end = c(2015, 12))
  }else {
    ts_data <- ts(monthly_sales[, 2], frequency = 12, 
                   start = c(2012, 01), end = c(2015, 12))
  }
  nValid = as.numeric(validation)
  nTrain = length(ts_data) - nValid
  train_data <- window(ts_data, start = c(2012, 1), end = c(2012, nTrain))
  test_data <- window(ts_data, start = c(2012, nTrain + 1), end = c(2012, nTrain + nValid))
  
  # Simple linear trend
  trend_fit <- tslm(train_data ~ trend)
  trend_pred <- forecast(trend_fit, h = nValid, level = 0)
  
  # Only seasonal Fit
  season_fit <- tslm(train_data ~ season)
  season_pred <- forecast(season_fit, h = nValid, level = 0)
  
  # add seasonality to trend
  season_trend_fit <- tslm(train_data ~ trend + season)
  season_trend_pred <- forecast(season_trend_fit, h = nValid, level = 0)
  
  # add polynomial trend only
  poly_trend_fit <- tslm(train_data ~ trend + I(trend)^2)
  ploy_trend_pred <- forecast(poly_trend_fit, h = nValid, level = 0)
  
  # add season to polynomial trend
  poly_st_fit <- tslm(train_data ~ trend + I(trend)^2 + season)
  poly_st_pred <- forecast(poly_st_fit, h = nValid, level = 0)
  
  # check with exponential trend
  exp_trend <- tslm(train_data ~ trend, lambda = 0)
  exp_pred <- forecast(exp_trend, h = nValid, level = 0)
  
  # exponential seasonality with trend
  exp_st <- tslm(train_data ~ trend + season, lambda = 0)
  exp_st_pred <- forecast(exp_st, h = nValid, level = 0)
  
  # exponential seasonality with polynomial trend
  exp_poly_season <- tslm(train_data ~ trend + I(trend)^2 + season, lambda = 0)
  exp_poly_season_pred <- forecast(exp_poly_season, h = nValid, level = 0)
  
  # Add seasonal naive as benchmark
  snaive_pred <- snaive(train_data, h = nValid, level = 0)
  
  # Add smoothing models
  # Simple Exponential Smoothing
  ses_model <- ets(train_data, model = "ANN")
  ses_model_pred <- forecast(ses_model, h = nValid, level = 0)
  
  # More smoothing models to come
  # hw, ARIMA
  hw_model <- hw(train_data, h = nValid)
  
  # ARIMA Model
  arima_model <- auto.arima(train_data)
  arima_pred <- forecast(arima_model, h = nValid, level = 0)
  
  # Computing accuracies
  trend_accr <- accuracy(trend_pred$mean, test_data)
  season_accr <- accuracy(season_pred$mean, test_data)
  season_trend_accr <- accuracy(season_trend_pred$mean, test_data)
  poly_trend_accr <- accuracy(ploy_trend_pred$mean, test_data)
  poly_st_accr <- accuracy(poly_st_pred$mean, test_data)
  exp_accr <- accuracy(exp_pred$mean, test_data)
  exp_st_accr <- accuracy(exp_st_pred$mean, test_data)
  exp_poly_season_accr <- accuracy(exp_poly_season_pred$mean, test_data)
  snaive_accr <- accuracy(snaive_pred$mean, test_data)
  ses_accr <- accuracy(ses_model_pred$mean, test_data)
  # done above
  hw_accr <- accuracy(hw_model$mean, test_data)
  arima_accr <- accuracy(arima_pred$mean, test_data)
  
  # Storing the results in a dataframe
  accr_df <- rbind.data.frame(trend_accr, season_accr, season_trend_accr, poly_trend_accr,
                  poly_st_accr, exp_accr, exp_st_accr, exp_poly_season_accr, 
                  snaive_accr, ses_accr, hw_accr, arima_accr)
  rownames(accr_df) <- c("Trend Accuracy", "Season Accuracy", "Trend with Season Accuracy",
    "Polynomial Trend Accuracy", "Polynomial Trend with Season Accuracy", 
    "Exponential Model Accuracy", "Exponential Season with Trend Accuracy", 
    "Exponential Polynomial Trend with Season Accuracy", "Seasonal Naive Accuracy", 
    "Simple Exponential Smoothing Accuracy", "Holt-Winter Model Accuracy", 
    "Arima Model Accuracy")
  
  return (accr_df)
  
}
