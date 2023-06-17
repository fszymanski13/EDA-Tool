#' @author Filip Szymański Kacper Sokołowski Piotr Radziszewski
#' The package contains useful functions for Exploratory Data Analysis
#' 
#' Check and Install Necessary Packages
#'
#' This function checks whether the necessary packages are installed in your R environment and 
#' installs them if they are not already installed.
#'
#' @export

list.of.packages <- c("installr","Rcpp","DescTools","stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("DescTools")
library("stats")
library("Rcpp")
library("installr")

#' Set Class
#'
#' This function creates a new class lomza that inherits from the base class data.frame.
#' @export

lomza <- setClass("lomza", contains = "data.frame")


#' Extended Summary
#'
#' This function provides a more detailed summary of the data than the base summary function.
#' It only applies to numeric columns and gives warning if there are any NA values in the column.
#' @param object An object of class "lomza".
#' @return A detailed summary of the data in "lomza" object.
#' @export

setGeneric(name = "summaryNEW", signature = "object",
           def = function(object) standardGeneric("summaryNEW"))

setMethod(f = "summaryNEW", signature = "lomza", definition = function(object) {
  num_cols <- which(sapply(object, is.numeric))
  num_data <- object[, num_cols]
  non_num_cols <- which(sapply(object, function(x) !is.numeric(x)))
  non_num_data <- object[, non_num_cols]
  
  ## Function to remove NA values and set a warning
  remove_NA <- function(x) {
    if (any(is.na(x))) {
      warning("NA values found in the column. They will be removed.")
      x <- x[!is.na(x)]
    }
    return(x)
  }
  
  ## Apply summary functions with NA removal
  Min <- round(apply(num_data, 2, function(x) min(remove_NA(x))), 2)
  Max <- apply(num_data, 2, function(x) max(remove_NA(x)))
  Mean <- apply(num_data, 2, function(x) mean(remove_NA(x)))
  Median <- apply(num_data, 2, function(x) median(remove_NA(x)))
  FirstQ <- apply(num_data, 2, function(x) quantile(remove_NA(x), 0.25))
  ThirdQ <- apply(num_data, 2, function(x) quantile(remove_NA(x), 0.75))
  IQR <- apply(num_data, 2, function(x) IQR(remove_NA(x)))
  meanTRIM10 <- apply(num_data, 2, function(x) {
    trimmed_x <- x[x >= quantile(remove_NA(x), 0.1) & x <= quantile(remove_NA(x), 0.9)]
    mean(remove_NA(trimmed_x))
  })
  meanTRIM20 <- apply(num_data, 2, function(x) {
    trimmed_x <- x[x >= quantile(remove_NA(x), 0.2) & x <= quantile(remove_NA(x), 0.8)]
    mean(remove_NA(trimmed_x))
  })
  MAD <- apply(num_data, 2, function(x) mad(remove_NA(x)))
  VAR <- apply(num_data, 2, function(x) var(remove_NA(x)))
  StDev <- apply(num_data, 2, function(x) sd(remove_NA(x)))
  Skewness <- apply(num_data, 2, function(x) Skew(remove_NA(x)))
  Kurtosis <- apply(num_data, 2, function(x) Kurt(remove_NA(x)))
  CV <- apply(num_data, 2, function(x) {
    trimmed_x <- remove_NA(x)
    sd(trimmed_x) / mean(trimmed_x) * 100
  })
  
  
  ## Create a matrix of the statistics for each numeric column
  stats_matrix <- matrix(nrow = 15, ncol = length(num_cols))
  colnames(stats_matrix) <- colnames(num_data)
  rownames(stats_matrix) <- c("Min:", "Max:", "Mean:", "Median:", "1st. Qu:", "3rd. QU:", "Var:", "StDev:", "CV:", "MAD:", "IQR:", "Skew:", "Kurt:", "MeanTrim0.1:", "MeanTrim0.2:")
  stats_matrix[1,] <- Min
  stats_matrix[2,] <- Max
  stats_matrix[3,] <- Mean
  stats_matrix[4,] <- Median
  stats_matrix[5,] <- FirstQ
  stats_matrix[6,] <- ThirdQ
  stats_matrix[7,] <- VAR
  stats_matrix[8,] <- StDev
  stats_matrix[9,] <- CV
  stats_matrix[10,] <- MAD
  stats_matrix[11,] <- IQR
  stats_matrix[12,] <- Skewness
  stats_matrix[13,] <- Kurtosis
  stats_matrix[14,] <- meanTRIM10
  stats_matrix[15,] <- meanTRIM20
  
  cat("Summary statistics for numeric data \n")
  print(round(stats_matrix, 2))
  cat("\nSummary statistics for non-numeric data \n")
  print(summary(non_num_data))
})

#' Chi-Squared Test
#'
#' This function performs a chi-squared test on all numeric columns of the "lomza" object.
#' It will give a warning if there are any NA values in the column.
#' @param object An object of class "lomza".
#' @return The chi-squared test results for each numeric column in the "lomza" object.
#' @export

setGeneric("chi_sq", function(object) standardGeneric("chi_sq"))

setMethod("chi_sq", "lomza", function(object) {
  
  numeric_cols <- sapply(object, is.numeric)
  
  chi_results <- lapply(seq_along(numeric_cols)[numeric_cols], function(i) {
    col <- object[[i]]
    
    # Check for NA values and print a warning if present
    if(any(is.na(col))) {
      warning(paste("Column", colnames(object)[i], "contains NA values. They will be removed for the chi-square test."))
      col <- col[!is.na(col)]  # Remove NA values
    }
    
    chisq_test <- chisq.test(col)
    list(
      Column = colnames(object)[i],
      Observed_Values = chisq_test$observed,
      Expected_Values = chisq_test$expected,
      Chi_Square = chisq_test$statistic,
      Degrees_of_Freedom = chisq_test$parameter,
      p_value = chisq_test$p.value
    )
  })
  
  do.call(rbind, chi_results)
})


#' T-Test
#'
#' This function performs a t-test on all numeric columns of the "lomza" object.
#' It will give a warning if there are any NA values in the column.
#' @param object An object of class "lomza".
#' @return The t-test results for each numeric column in the "lomza" object.
#' @export

setGeneric("t_test", function(object) standardGeneric("t_test"))

setMethod("t_test", "lomza", function(object) {
  
  numeric_cols <- sapply(object, is.numeric)
  
  t_results <- lapply(seq_along(numeric_cols)[numeric_cols], function(i) {
    col <- object[[i]]
    
    # Check for NA values and print a warning if present
    if(any(is.na(col))) {
      warning(paste("Column", colnames(object)[i], "contains NA values. They will be removed for the t-test."))
      col <- col[!is.na(col)]  # Remove NA values
    }
    
    t_test <- t.test(col)
    list(
      Column = colnames(object)[i],
      Mean = mean(col),
      p_value = t_test$p.value,
      t_statistic = t_test$statistic,
      Test = "t-test"
    )
  })
  
  do.call(rbind, t_results)
})


#' Pearson's Correlation Coefficient
#'
#' This function calculates the Pearson's correlation coefficient between every pair of numeric 
#' columns in the "lomza" object. It will give a warning if there are any NA values in the columns.
#' @param object An object of class "lomza".
#' @return The correlation coefficient between every pair of numeric columns in the "lomza" object.
#' @export

setGeneric("pearson_correlation", function(object) standardGeneric("pearson_correlation"))

setMethod("pearson_correlation", "lomza", function(object) {
  
  numeric_cols <- sapply(object, is.numeric)
  
  correlation_results <- combn(colnames(object)[numeric_cols], 2, function(cols) {
    col1 <- object[[cols[1]]]
    col2 <- object[[cols[2]]]
    
    # Check for NA values and print a warning if present
    if(any(is.na(col1)) || any(is.na(col2))) {
      warning(paste("Columns", cols[1], "and/or", cols[2], "contain NA values. They will be removed for the correlation calculation."))
      na_rows <- is.na(col1) | is.na(col2)  # Rows where either column has NA
      col1 <- col1[!na_rows]  # Remove NA values
      col2 <- col2[!na_rows]  # Remove NA values
    }
    
    correlation <- cor(col1, col2, method = "pearson")
    list(
      Variable_1 = cols[1],
      Variable_2 = cols[2],
      Correlation = correlation
    )
  }, simplify = FALSE)
  
  do.call(rbind, correlation_results)
})


#' Data Analysis
#'
#' This function performs a chi-squared test, t-test, and calculates the Pearson's correlation 
#' coefficient for all relevant numeric columns in the "lomza" object.
#' @param object An object of class "lomza".
#' @return The results of the chi-squared test, t-test, and Pearson's correlation coefficient 
#' calculation for the "lomza" object.
#' @export

setGeneric("analyze_data", function(object) standardGeneric("analyze_data"))

setMethod("analyze_data", "lomza", function(object) {
  
  # Chi-Squared Test
  chi_sq_results <- chi_sq(object)
  print("Results of Chi-Squared Test:")
  print(chi_sq_results)
  
  # T-Test
  t_test_results <- t_test(object)
  cat("\n")
  cat("\n")
  print("Results of T-Test:")
  print(t_test_results)
  
  # Pearson's Correlation Coefficient
  correlation_results <- pearson_correlation(object)
  cat("\n")
  cat("\n")
  print("Results of Pearson's Correlation Coefficient:")
  print(correlation_results)
  
})


#' Fill Missing Values for Numeric Columns
#'
#' This function provides multiple methods to handle missing values in numeric columns of the 
#' "lomza" object. The methods include deleting the row or column containing the missing value, 
#' filling the missing value with mean, median, or mode of the column, or filling with a 
#' specified constant value.
#' @param object An object of class "lomza".
#' @return The "lomza" object with missing values in numeric columns filled.
#' @export

setGeneric("fill_numeric", function(object, method) {
  standardGeneric("fill_numeric")
})

setMethod("fill_numeric", "lomza",
          function(object, method) {
            if (!is.character(method) && !is.list(method)) {
              stop("Invalid method. Must be a character string or a list")
            }
            
            if (is.character(method)) {
              if (!method %in% c("del_row", "del_col", "mean", "median", "mode")) {
                stop("Invalid method. Choose from 'del_row', 'del_col', 'mean', 'median', 'mode'")
              }
              for (name in names(object)) {
                if (is.numeric(object[[name]]) && any(is.na(object[[name]]))) {
                  if (method == "del_row") {
                    object <- object[!is.na(object[[name]]),]
                  } else if (method == "del_col") {
                    object[[name]] <- NULL
                  } else if (method == "mean") {
                    object[[name]][is.na(object[[name]])] <- mean(object[[name]], na.rm = TRUE)
                  } else if (method == "median") {
                    object[[name]][is.na(object[[name]])] <- median(object[[name]], na.rm = TRUE)
                  } else if (method == "mode") {
                    uniqv <- unique(object[[name]])
                    object[[name]][is.na(object[[name]])] <- uniqv[which.max(tabulate(match(object[[name]], uniqv)))]
                  }
                }
              }
            } else if (is.list(method)) {
              if (length(method) != 2) {
                stop("Invalid method list. Must have length 2")
              }
              constant <- method[[1]]
              value <- method[[2]]
              if (!is.character(constant)) {
                stop("Invalid value in method list. First element must be character string")
              }
              if (!is.numeric(value)) {
                stop("Invalid value in method list. Second element must be numeric")
              }
              if (constant!="constant") {
                stop("Invalid method. For list input only 'constant' method is allowed")
              }
              for (name in names(object)) {
                if (is.numeric(object[[name]]) && any(is.na(object[[name]]))) {
                  object[[name]][is.na(object[[name]])] <- value
                }
              }
            }
            return(object)
          })


#' Fill Missing Values for Non-numeric Columns
#'
#' This function provides multiple methods to handle missing values in non-numeric columns of the 
#' "lomza" object. The methods include deleting the row or column containing the missing value, 
#' filling the missing value with the most frequent value, or filling with a specified constant value.
#' @param object An object of class "lomza".
#' @return The "lomza" object with missing values in non-numeric columns filled.
#' @export

setGeneric("fill_non_numeric", function(object, method) {
  standardGeneric("fill_non_numeric")
})

setMethod("fill_non_numeric", "lomza",
          function(object, method) {
            if (!is.character(method) && !is.list(method)) {
              stop("Invalid method. Must be a character string or a list")
            }
            if (is.character(method)) {
              if (!method %in% c("del_row", "del_col")) {
                stop("Invalid method. Choose from 'del_row', 'del_col'")
              }
              for (name in names(object)) {
                if (!is.numeric(object[[name]]) && any(is.na(object[[name]]))) {
                  if (method == "del_row") {
                    object <- object[!is.na(object[[name]]),]
                  } else if (method == "del_col") {
                    object[[name]] <- NULL
                  }
                }
              }
            } else if (is.list(method)) {
              if (is.list(method) && length(method) != 2) {
                stop("Invalid method list. Must have length 2")
              }
              constant <- method[[1]]
              value <- method[[2]]
              if (!is.character(constant)) {
                stop("Invalid value in method list. First element must be character string")
              }
              if (!is.character(value)) {
                stop("Invalid value in method list. Second element must be a string")
              }
              if (constant!="constant") {
                stop("Invalid method. For list input only 'constant' method is allowed")
              }
              for (name in names(object)) {
                if (!is.numeric(object[[name]]) && any(is.na(object[[name]]))) {
                  object[[name]][is.na(object[[name]])] <- value
                }
              }
            }
            return(object)
          })


#' Count Missing Values
#'
#' This function counts the number of missing values in each column of the "lomza" object and 
#' returns a report.
#' @param object An object of class "lomza".
#' @return A report detailing the number of missing values in each column of the "lomza" object.
#' @export

cppFunction("
int count_na(NumericVector x) {
  int n = x.size();
  int count = 0;
  for (int i = 0; i < n; ++i) {
    if (NumericVector::is_na(x[i])) count++;
  }
  return count;
}")

cppFunction("
int count_na_char(CharacterVector x) {
  int n = x.size();
  int count = 0;
  for (int i = 0; i < n; ++i) {
    if (CharacterVector::is_na(x[i])) count++;
  }
  return count;
}")


setGeneric("missing_report", function(object) {
  standardGeneric("missing_report")
})

setMethod("missing_report", "lomza",
          function(object) {
            report <- data.frame(matrix(ncol = 4, nrow = 0))
            colnames(report) <- c("Column Name", "Full Values", "Missing Values", "Percentage Missing")
            for (name in names(object)) {
              total <- length(object[[name]])
              if (is.numeric(object[[name]])) {
                missing <- count_na(object[[name]])
              } else {
                missing <- count_na_char(object[[name]])
              }
              full <- total - missing
              percent_missing <- (missing / total) * 100
              report <- rbind(report, data.frame("Column Name" = name, "Full Values" = full, "Missing Values" = missing, "Percentage Missing" = percent_missing))
            }
            print(report)
          })

