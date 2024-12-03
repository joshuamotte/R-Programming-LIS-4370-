# replace na values
replace_na <- function(data, method = "count") {
  if (!is.data.frame(data))
    stop("You must use a data frame.")

  if (method == "count") {
    return(sum(is.na(data)))
  } else if (method == "mean") {
    replace_na_with_mean <- function(x) {
      if (is.numeric(x)) {
        na_indices <- which(is.na(x))
        x[na_indices] <- mean(x, na.rm = TRUE)
      }
      return(x)
    }
    return(data.frame(lapply(data, replace_na_with_mean)))
  } else if (method == "median") {
    replace_na_with_median <- function(x) {
      if (is.numeric(x)) {
        na_indices <- which(is.na(x))
        x[na_indices] <- median(x, na.rm = TRUE)
      }
      return(x)
    }
    return(data.frame(lapply(data, replace_na_with_median)))
  } else if (method == "impute") {
    impute_na <- function(x) {
      if (is.numeric(x)) {
        na_indices <- which(is.na(x))
        x[na_indices] <- sample(x[!is.na(x)], length(na_indices), replace = TRUE)
      }
      return(x)
    }
    return(data.frame(lapply(data, impute_na)))
  } else {
    stop("Invalid method. Use 'count', 'mean', 'median', or 'impute'.")
  }
}

# replace outliers

replace_outliers <- function(data, method = "show", threshold = 3) {
  if (!is.data.frame(data))
    stop("You must use a data frame.")

  detect_outliers <- function(x) {
    if (is.numeric(x)) {
      mean_x <- mean(x, na.rm = TRUE)
      sd_x <- sd(x, na.rm = TRUE)
      outliers <- abs(x - mean_x) > threshold * sd_x
      return(outliers)
    } else {
      return(rep(FALSE, length(x)))
    }
  }

  if (method == "show") {
    outlier_list <- lapply(data, function(column) {
      if (is.numeric(column)) {
        outliers <- column[detect_outliers(column)]
        outliers <- outliers[!is.na(outliers)]
        if (length(outliers) > 0) {
          return(outliers)
        } else {
          return("None")
        }
      } else {
        return("Not numeric")
      }
    })
    return(outlier_list)
  } else if (method == "remove") {
    remove_outliers <- function(x) {
      if (is.numeric(x)) {
        outlier_indices <- detect_outliers(x)
        x[outlier_indices] <- NA
      }
      return(x)
    }
    return(data.frame(lapply(data, remove_outliers)))
  } else {
    stop("Invalid method. Use 'show' or 'remove'.")
  }
}

#correlation

correlation_analysis <- function(data, target_var = NULL) {
  if (!is.data.frame(data))
    stop("You must use a data frame.")

  cor_matrix <- cor(data, use = "complete.obs", method = "pearson")

  if (!is.null(target_var)) {
    if (!target_var %in% colnames(data))
      stop("Target variable not in data.")
    return(sort(cor_matrix[, target_var], decreasing = TRUE))
  }

  cor_matrix[upper.tri(cor_matrix)] <- NA

  cor_df <- as.data.frame(as.table(cor_matrix))

  cor_df <- na.omit(cor_df)

  cor_df <- cor_df[cor_df$Var1 != cor_df$Var2, ]
  cor_df <- cor_df[order(abs(cor_df$Freq), decreasing = TRUE), ]

  print(cor_df)

  ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
    geom_tile() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
    theme(plot.title = element_text(hjust = 0.5))
}

# regression


regression_analysis <- function(data, target_var) {
  if (!is.data.frame(data))
    stop("You must use a data frame.")

  # target variable
  if (!(target_var %in% colnames(data)))
    stop("Target variable not in the data.")

  # split data 80/20 for training and testing
  set.seed(123)
  num_rows <- nrow(data)
  rand_index <- sample(1:num_rows)
  cutpoint <- round(num_rows * 0.8)

  train_data <- data[rand_index[1:cutpoint], ]
  test_data <- data[rand_index[(cutpoint + 1):num_rows], ]

  # train models
  formula <- as.formula(paste(target_var, "~ ."))
  lm_model <- lm(formula, data = train_data)
  svm_model <- svm(formula, data = train_data)
  ksvm_model <- ksvm(formula, data = train_data)
  rf_model <- randomForest(formula, data = train_data)

  # predict
  lm_pred <- predict(lm_model, test_data)
  svm_pred <- predict(svm_model, test_data)
  ksvm_pred <- predict(ksvm_model, test_data)
  rf_pred <- predict(rf_model, test_data)

  # RMSE
  lm_rmse <- RMSE(lm_pred, test_data[[target_var]])
  svm_rmse <- RMSE(svm_pred, test_data[[target_var]])
  ksvm_rmse <- RMSE(ksvm_pred, test_data[[target_var]])
  rf_rmse <- RMSE(rf_pred, test_data[[target_var]])

  # MAE
  lm_mae <- MAE(lm_pred, test_data[[target_var]])
  svm_mae <- MAE(svm_pred, test_data[[target_var]])
  ksvm_mae <- MAE(ksvm_pred, test_data[[target_var]])
  rf_mae <- MAE(rf_pred, test_data[[target_var]])

  # R-squared
  lm_r2 <- cor(lm_pred, test_data[[target_var]])^2
  svm_r2 <- cor(svm_pred, test_data[[target_var]])^2
  ksvm_r2 <- cor(ksvm_pred, test_data[[target_var]])^2
  rf_r2 <- cor(rf_pred, test_data[[target_var]])^2

  # comparison table
  model_comparison <- data.frame(
    Model = c("LM", "SVM", "KSVM", "RF"),
    RMSE = c(lm_rmse, svm_rmse, ksvm_rmse, rf_rmse),
    MAE = c(lm_mae, svm_mae, ksvm_mae, rf_mae),
    R_Squared = c(lm_r2, svm_r2, ksvm_r2, rf_r2)
  )

  # order by RMSE
  model_comparison <- model_comparison[order(model_comparison$RMSE), ]
  print(model_comparison)

}

# regression plots

regression_analysis_plots <- function(data, target_var, predictor_var, model_choice) {
  if (!is.data.frame(data)) stop("You must use a data frame.")

  # target and predictor variables
  if (!(target_var %in% colnames(data)))
    stop("Target variable not in the data.")
  if (!(predictor_var %in% colnames(data)))
    stop("Predictor variable not in the data.")

  # split data 80/20 for training and testing
  set.seed(123)
  num_rows <- nrow(data)
  rand_index <- sample(1:num_rows)
  cutpoint <- round(num_rows * 0.8)

  train_data <- data[rand_index[1:cutpoint], ]
  test_data <- data[rand_index[(cutpoint + 1):num_rows], ]

  # train models
  formula <- as.formula(paste(target_var, "~", predictor_var))
  lm_model <- lm(formula, data = train_data)
  svm_model <- svm(formula, data = train_data)
  ksvm_model <- ksvm(formula, data = train_data)
  rf_model <- randomForest(formula, data = train_data)

  # predictions
  lm_pred <- predict(lm_model, test_data)
  svm_pred <- predict(svm_model, test_data)
  ksvm_pred <- predict(ksvm_model, test_data)
  rf_pred <- predict(rf_model, test_data)

  # RMSE and MAE calculations
  lm_rmse <- RMSE(lm_pred, test_data[[target_var]])
  svm_rmse <- RMSE(svm_pred, test_data[[target_var]])
  ksvm_rmse <- RMSE(ksvm_pred, test_data[[target_var]])
  rf_rmse <- RMSE(rf_pred, test_data[[target_var]])

  lm_mae <- MAE(lm_pred, test_data[[target_var]])
  svm_mae <- MAE(svm_pred, test_data[[target_var]])
  ksvm_mae <- MAE(ksvm_pred, test_data[[target_var]])
  rf_mae <- MAE(rf_pred, test_data[[target_var]])

  # comparison table
  model_comparison <- data.frame(
    Model = c("LM", "SVM", "KSVM", "RF"),
    RMSE = c(lm_rmse, svm_rmse, ksvm_rmse, rf_rmse),
    MAE = c(lm_mae, svm_mae, ksvm_mae, rf_mae)
  )
  model_comparison <- model_comparison[order(model_comparison$RMSE), ]

  # absolute errors
  lm_abs_error <- abs(test_data[[target_var]] - lm_pred)
  svm_abs_error <- abs(test_data[[target_var]] - svm_pred)
  ksvm_abs_error <- abs(test_data[[target_var]] - ksvm_pred)
  rf_abs_error <- abs(test_data[[target_var]] - rf_pred)

  # ggplots for each model
  plot_model <- function(model_name, model_pred, abs_error) {
    ggplot(data = test_data, aes_string(x = predictor_var, y = target_var)) +
      geom_point(aes(size = abs_error, color = abs_error)) +
      labs(title = paste(model_name, "with Absolute Error"),
           x = predictor_var, y = target_var) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
  }

  # choose which model to plot based on user input
  if (model_choice == "LM") {
    plot <- plot_model("Linear Model", lm_pred, lm_abs_error)
  } else if (model_choice == "SVM") {
    plot <- plot_model("SVM", svm_pred, svm_abs_error)
  } else if (model_choice == "KSVM") {
    plot <- plot_model("KSVM", ksvm_pred, ksvm_abs_error)
  } else if (model_choice == "RF") {
    plot <- plot_model("Random Forest", rf_pred, rf_abs_error)
  } else {
    stop("Invalid model choice. Please choose 'LM', 'SVM', 'KSVM', or 'RF'.")
  }

  print(plot)
}




