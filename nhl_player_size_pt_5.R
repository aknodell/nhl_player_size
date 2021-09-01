log_loss <- function(prob, actual) {
  log_losses <-
    -1 * ((actual * log(prob)) + ((1 - actual) * log(1 - prob)))
  
  log_losses[is.nan(log_losses)] <- 0
  
  mean(log_losses)
}

lambda <- 10 ^ seq(-3, 3, length = 100)

data <-
  team_game_diffs %>%
  dplyr::filter(team == "home", strength_state == "All") %>% 
  dplyr::mutate(
    win = g_diff %>% magrittr::is_greater_than(0) %>% as.numeric()
    ) %>% 
  dplyr::select(win, tidyr::matches("(days_on_earth|[hw]eight|bmi).*_diff"))

set.seed(123)

training_samples <- data$win %>% caret::createDataPartition(p = 0.8, list = F)

data_train <- data[training_samples,]

data_test <- data[-training_samples,]

#####
## GLM Logistic
###

logistic_models <-
  purrr::map(
    c("NA", "F", "D", "Both", ".+"),
    function(select) {
      set.seed(123)
      
      model <-
        glm(
          win ~ .,
          family = "binomial",
          data = 
            data_train %>%
            dplyr::select(
              win,
              tidyselect::matches(
                stringr::str_c(select, "_.+_diff")
              )
            )
        )
      
      predictions <- model %>% predict(data_test, type = "response")
      
      list(
        Model = list(model),
        model_name = stringr::str_c("logistic_", select),
        RMSE_test = caret::RMSE(predictions, data_test$win),
        RSquare_test = caret::R2(predictions, data_test$win),
        log_loss_test = log_loss(predictions, data_test$win)
      )
    }
  ) %>%
  purrr::set_names(c("NA", "F", "D", "Both", "All")) 
  dplyr::bind_rows()


purrr::map(
  c("NA", "F", "D", "Both", "[DF]"),
  function(select) {
    set.seed(123)
    
    model <-
      glm(
        win ~ .,
        family = "binomial",
        data = 
          data_train %>%
          dplyr::select(
            -c(tidyselect::matches("(D|Both)_(D|Both)_diff"))
          )
      )
    
    predictions <- model %>% predict(data_test, type = "response")
    
    list(
      Model = list(model),
      model_name = stringr::str_c("logistic_", select),
      RMSE_test = caret::RMSE(predictions, data_test$win),
      RSquare_test = caret::R2(predictions, data_test$win),
      log_loss_test = log_loss(predictions, data_test$win)
    )
  }
) %>%
  purrr::set_names(c("NA", "F", "D", "Both", "All")) %>%
  dplyr::bind_rows()

#####
## Ridge
###

get_ridge_model <- function() {
  set.seed(123)
  
  ridge <-
    caret::train(
      win ~ .,
      data = data_train,
      method = "glmnet",
      trControl = caret::trainControl("cv", number = 10),
      tuneGrid = expand.grid(alpha = 0, lambda = lambda)
    )
  
  coef(ridge$finalModel, ridge$bestTune$lambda) %>% print()
  
  predictions <- ridge %>% predict(data_test)
  
  list(
    Model = list(ridge),
    model_name = "ridge",
    RMSE_test = RMSE(predictions, data_test$win),
    RSquare_test = R2(predictions, data_test$win),
    log_loss_test = log_loss(predictions, data_test$win)
  )
}

ridge <- get_ridge_model()

#####
## Lasso
###

get_lasso_model <- function() {
  set.seed(123)
  
  lasso <- 
    caret::train(
      win ~ .,
      data = data_train,
      method = "glmnet",
      trControl = caret::trainControl("cv", number = 10),
      tuneGrid = expand.grid(alpha = 1, lambda = lambda)
    )
  
  coef(lasso$finalModel, lasso$bestTune$lambda) %>% print()
  
  predictions <- lasso %>% predict(data_test)
  
  list(
    Model = list(lasso),
    model_name = "lasso",
    RMSE_test = RMSE(predictions, data_test$win),
    RSquare_test = R2(predictions, data_test$win),
    log_loss_test = log_loss(predictions, data_test$win)
  )
}

lasso <- get_lasso_model()

#####
## Elastic Net
###

get_elastic_net <- function() {
  set.seed(123)
  
  elastic <-
    caret::train(
      win ~ .,
      data = data_train,
      method = "glmnet",
      trControl = caret::trainControl("cv", number = 10),
      tuneLength = 10
    )
  
  coef(elastic$finalModel, elastic$bestTune$lambda) %>% print()
  
  predictions <- elastic %>% predict(data_test)
  
  list(
    Model = list(elastic),
    model_name = "elastic_net",
    RMSE_test = RMSE(predictions, data_test$win),
    RSquare_test = R2(predictions, data_test$win),
    log_loss_test = log_loss(predictions, data_test$win)
  )
}

elastic_net <- get_elastic_net()

#####
## XGBoost
###

data_train_xgb <- data_train %>% model.matrix(object = win ~ .)

data_test_xgb <- data_test %>% model.matrix(object = win ~ .,)

get_xgboost_model <- function() {
  set.seed(123)
  
  xgb_grid <-
    tidyr::expand_grid(
      nrounds = 2:10,
      eta = seq(0, 1, by = 0.05),
      max_depth = 1:5,
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    )
  
  xgb_trcontrol_1 = trainControl(
    method = "cv",
    number = 10
  )
  
  xgboost_train <-
    caret::train(
      x = data_train_xgb,
      y = data_train$win,
      trControl = xgb_trcontrol_1,
      tuneGrid = xgb_grid,
      method = "xgbTree"
    )
  
  varImp(xgboost_train, scale = F) %>% print()
  
  predictions <- predict(xgboost_train, data_test_xgb)
  
  list(
    Model = list(xgboost_train),
    model_name = "xgboost",
    RMSE_test = RMSE(predictions, data_test$win),
    RSquare_test = R2(predictions, data_test$win),
    log_loss_test = log_loss(predictions, data_test$win)
  )
}

xgboost_model <- get_xgboost_model()

#####
## Charts and Analysis
###

dplyr::bind_rows(
  logistic_models %>%
    dplyr::bind_rows(),
  ridge,
  lasso,
  elastic_net,
  xgboost_model
) %>%
  dplyr::arrange(log_loss_test)

logistic_coefs <-
  purrr::map2(
    logistic_models,
    names(logistic_models),
    function(model, name) {
      model <- model$Model %>% purrr::flatten()
      
      coefs <- model$coefficients
      
      tibble::tibble(
        predictor = names(coefs),
        coef = coefs,
        name = name
      ) %>%
        dplyr::mutate()
    }
  ) %>%
  dplyr::bind_rows()

penalized_coefs <-
  purrr::map2(
    list(ridge, elastic_net, lasso),
    c("ridge", "elastic_net", "lasso"),
    function(x, name) {
      coef(
        x %>%
          purrr::pluck("Model") %>%
          purrr::flatten() %>%
          .$finalModel, 
        x %>% 
          purrr::pluck("Model") %>% 
          purrr::flatten() %>% 
          .$bestTune %>% 
          .$lambda
      ) %>% 
        as.matrix() %>%
        tibble::as_tibble(rownames = "predictor") %>%
        dplyr::rename(coef = s1) %>%
        dplyr::mutate(name = name)
    }
  ) %>%
  dplyr::bind_rows()


logistic_coefs %>% 
  dplyr::filter(name == "All", !is.na(coef), predictor != "(Intercept)") %>%
  dplyr::mutate(
    stat = 
      predictor %>% 
      stringr::str_remove("(_[^_]+){2}_diff") %>%
      stringr::str_replace_all("_", " ") %>%
      stringr::str_to_title() %>%
      stringr::str_replace("Bmi", "BMI") %>%
      factor(levels = c("Days On Earth", "Height", "Weight", "BMI")),
    `Away Position` = 
      predictor %>% 
      stringr::str_extract("_[^_]+_diff") %>%
      stringr::str_remove_all("_|diff"),
    `Home Position` = 
      predictor %>% 
      stringr::str_extract("[^_]+_[^_]+_diff") %>%
      stringr::str_remove_all("_[^_]+_diff") %>%
      factor(levels = c("F", "D", "Both")),
    Coefficient = coef
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = `Away Position`, y = Coefficient, color = `Home Position`)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.5)) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(stat ~ ., ncol = 1, scales = "fixed") +
  ggplot2::ggtitle("Logistic Regression Coefficients", "All Predictors")

logistic_coefs %>% 
  dplyr::filter(name %in% c("F", "D", "Both"), predictor != "(Intercept)") %>%
  dplyr::mutate(
    stat = 
      predictor %>% 
      stringr::str_remove("(_[^_]+){2}_diff") %>%
      stringr::str_replace_all("_", " ") %>%
      stringr::str_to_title() %>%
      stringr::str_replace("Bmi", "BMI") %>%
      factor(levels = c("Days On Earth", "Height", "Weight", "BMI")),
    `Away Position` = 
      predictor %>% 
      stringr::str_extract("_[^_]+_diff") %>%
      stringr::str_remove_all("_|diff"),
    `Home Position` = factor(name, levels = c("F", "D", "Both")),
    Coefficient = coef
  ) 
  ggplot2::ggplot(ggplot2::aes(x = `Away Position`, y = Coefficient, color = `Home Position`)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.5)) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(stat ~ ., ncol = 1, scales = "fixed") +
  ggplot2::ggtitle("Logistic Regression Coefficients", "Home Positon Specifc Models")

penalized_coefs %>%
  dplyr::filter(predictor != "(Intercept)") %>%
  dplyr::mutate(
    stat = 
      predictor %>% 
      stringr::str_remove("(_[^_]+){2}_diff") %>%
      stringr::str_replace_all("_", " ") %>%
      stringr::str_to_title() %>%
      stringr::str_replace("Bmi", "BMI") %>%
      factor(levels = c("Days On Earth", "Height", "Weight", "BMI")),
    `Away Position` = 
      predictor %>% 
      stringr::str_extract("_[^_]+_diff") %>%
      stringr::str_remove_all("_|diff"),
    `Home Position` = 
      predictor %>% 
      stringr::str_extract("[^_]+_[^_]+_diff") %>%
      stringr::str_remove_all("_[^_]+_diff") %>%
      factor(levels = c("F", "D", "Both")),
    Coefficient = coef,
    Model = 
      name %>%
      stringr::str_replace("_", " ") %>%
      stringr::str_to_title() %>%
      factor(levels = c("Ridge", "Lasso", "Elastic Net"))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = `Away Position`, y = Coefficient, color = `Home Position`)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip() +
  ggplot2::facet_grid(stat ~ Model, switch = "y") +
  ggplot2::ggtitle("Penalized Regression Coefficients")

xgb_info_gain <-
  ## taken from print statement in get_xgboost_model()
  tibble::tribble(
    ~predictor, ~information_gain,
    "weight_F_D_diff",              0.12755,
    "height_Both_Both_diff",        0.12136,
    "days_on_earth_D_D_diff",       0.08962,
    "days_on_earth_Both_F_diff",    0.08283,
    "height_F_F_diff",              0.07978,
    "bmi_D_D_diff",                 0.07592,
    "days_on_earth_Both_Both_diff", 0.06363,
    "height_Both_D_diff",           0.06357,
    "height_F_Both_diff",           0.05367,
    "days_on_earth_D_F_diff",       0.05310,
    "days_on_earth_F_Both_diff",    0.04933,
    "height_F_D_diff",              0.04646,
    "height_D_Both_diff",           0.04599,
    "weight_D_F_diff",              0.02546,
    "height_D_D_diff",              0.02173,
    "bmi_D_Both_diff",              0.00000,
    "weight_F_Both_diff",           0.00000,
    "height_D_F_diff",              0.00000,
    "bmi_F_D_diff",                 0.00000,
    "bmi_Both_Both_diff",           0.00000
  )

xgb_info_gain %>%
  dplyr::mutate(
    value_label = information_gain %>% round(3) %>% as.character(),
    predictor = factor(predictor, levels = dplyr::pull(., predictor) %>% rev())
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = predictor, y = information_gain, fill = 1)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(ggplot2::aes(
    label = value_label, hjust = -0.1)
  ) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::scale_y_continuous("Information Gain", limits = c(0, 0.15)) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ggtitle("XGBoost Model Information Gain") +
  ggplot2::xlab("Predictor")

dplyr::bind_rows(
  logistic_models %>%
    dplyr::bind_rows(),
  ridge,
  lasso,
  elastic_net,
  xgboost_model
) %>%
  dplyr::select(model_name:log_loss_test) %>%
  tidyr::pivot_longer(
    cols = RMSE_test:log_loss_test, names_to = "metric", values_to = "value"
  ) %>%
  dplyr::group_by(metric) %>%
  dplyr::mutate(
    color_value = 
      value %>%
      magrittr::subtract(min(value, na.rm = T)) %>% 
      magrittr::divide_by(max(value, na.rm = T) - min(value, na.rm = T)),
    # color_value =
    #   ifelse(metric == "log_loss", color_value - 1, color_value)
    value = value %>% formatC(digits = 3, format = "fg", flag = "#"),
    Metric = 
      metric %>% 
      stringr::str_remove("_test") %>%
      stringr::str_replace("log_loss", "Log Loss") %>%
      factor(levels = c("Log Loss", "RSquare", "RMSE")),
    Model = 
      model_name %>%
      stringr::str_replace_all(
        c(
          "xgboost" = "XGBoost",
          "ridge" = "Ridge",
          "lasso" = "Lasso",
          "elastic_net" = "Elastic Net",
          "logistic_NA" = "Logistic (Intercept Only)",
          "logistic_F" = "Logistic (F Only)",
          "logistic_D" = "Logistic (D Only)",
          "logistic_Both" = "Logistic (Both Only)",
          "logistic_\\.\\+" = "Logistic (All)"
        )
      ) %>%
      factor(
        levels = 
          c(
            "XGBoost", 
            "Logistic (D Only)", 
            "Logistic (Both Only)", 
            "Logistic (F Only)", 
            "Logistic (Intercept Only)",
            "Lasso",
            "Logistic (All)",
            "Ridge",
            "Elastic Net"
          )
      )
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = Metric, y = Model, label = value)) +
  ggplot2::geom_text() +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_discrete(position = "top") +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank()) +
  ggplot2::xlab("") +
  ggplot2::ylab("") +
  ggplot2::ggtitle("Model Evaluation Comparisons")
  
  
