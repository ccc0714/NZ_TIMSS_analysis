library(tidyverse)
#Make sure the database XY has been created before calculating ATE due to incorporating survey weights 

# 1. Define the calculation function
calculate_ate <- function(model_list, treatment_labels, weights, n_subject = 2, n_pv = 5) {
  n_treat <- length(treatment_labels)
  
  # Initialize empty data frame
  res_df <- data.frame(
    treatment = character(n_subject * n_treat),
    subject = character(n_subject * n_treat),
    ATE = numeric(n_subject * n_treat),
    lower_quantile = numeric(n_subject * n_treat),
    upper_quantile = numeric(n_subject * n_treat),
    stringsAsFactors = FALSE
  )
  
  index <- 1
  weights <- weights / sum(weights)   
  
  for (i in 1:n_subject) {
    for (j in 1:n_treat) {
      
      ate_pv <- numeric(n_pv)
      var_pv <- numeric(n_pv)
      
      for (m in 1:n_pv) {
        tau_forest <- list(
          model_list[[m]]$predictions_tau1,
          model_list[[m]]$predictions_tau2,
          model_list[[m]]$predictions_tau3,
          model_list[[m]]$predictions_tau4,
          model_list[[m]]$predictions_tau5
        )
        
        tau <- tau_forest[[j]][, i, ]
        
        # Weighted ATE calculation
        ate_draws <- apply(tau, 2, function(x) sum(weights * x))
        
        ate_pv[m] <- mean(ate_draws)
        var_pv[m] <- mean((ate_draws - ate_pv[m])^2)
      }
      
      # Rubin's Rules for SE
      ate_bar <- mean(ate_pv)
      W <- mean(var_pv)
      B <- var(ate_pv)
      T_var <- W + (1 + 1 / n_pv) * B
      se <- sqrt(T_var)
      
      # Store results
      res_df$ATE[index] <- ate_bar
      res_df$lower_quantile[index] <- ate_bar - 1.96 * se
      res_df$upper_quantile[index] <- ate_bar + 1.96 * se
      res_df$subject[index] <- ifelse(i == 1, "Math", "Science")
      res_df$treatment[index] <- treatment_labels[j]
      
      index <- index + 1
    }
  }
  return(res_df)
}

labels_Q20 <- c(
  "Observing student as they work",
  "Asking students to answer questions during class",
  "Short, regular written assessments",
  "Longer test (unit tests or exams)",
  "Long-term projects"
)

labels_Q19C <- c(
  "Correct assignment + feedback",
  "Student correct own homework",
  "Discuss homework in class",
  "Monitor homework completion",
  "Homework contributes to grades"
)

labels_hw <- c(
  "Less than 15 minutes",
  "16–30 minutes",
  "Greater than 30 minutes",
  "At least 3 to 4 times a week",
  "1 to 2 times a week"
)

#Homework frequency and duration
model_files <- paste0(
  "yourdirectory\\ModelResultsChain",
  1:5,
  ".RData"
)

models <- vector("list", 5)
for (m in 1:5) {
  load(model_files[m])   # loads object: my_mod
  models[[m]] <- my_mod
}

ate_hw_results <- calculate_ate(models, labels_hw, XY$TOTWGT.x)
view(ate_hw_results)

#Question 20
model_files <- paste0(
  "yourdirectory\\ModelResultsNoHW",
  1:5,
  ".RData"
)

models <- vector("list", 5)
for (m in 1:5) {
  load(model_files[m])   # loads object: my_mod
  models[[m]] <- my_mod
}

ate_q20_results <- calculate_ate(models, labels_Q20, XY$TOTWGT.x)

#Question 19C 
model_files <- paste0(
  "yourdirectory\\Q19CModelChain",
  1:5,
  ".RData"
)

models <- vector("list", 5)
for (m in 1:5) {
  load(model_files[m])   # loads object: my_mod
  models[[m]] <- my_mod
}

ate_q19c_results <- calculate_ate(models, labels_Q19C, XY$TOTWGT.x)

#--------------2023 Frequency ATE------------------------------
#Make sure to run the 2023 XY data before calculating ATE 
#2023 XY data can be loaded from the file TIMSS2023_NZ_analysis.R
#The code below can be run for both student and teacher analysis of homework frequency in 2023
model_files <- paste0(
  "yourdirectory\\ModelResultsChain",
  1:5,
  ".RData"
)

models <- vector("list", 5)

for (m in 1:5) {
  load(model_files[m])        
  models[[m]] <- my_mod
}

n_subject <- 2
n_treat <- 4
n_pv <- 5

weights <- XY$TOTWGT.x
weights <- weights / sum(weights)

ate_df <- data.frame(
  treatment = character(n_subject * n_treat),
  subject = character(n_subject * n_treat),
  ATE = numeric(n_subject * n_treat),
  lower_quantile = numeric(n_subject * n_treat),
  upper_quantile = numeric(n_subject * n_treat)
)

index <- 1

for (i in 1:n_subject) {     # Math / Science
  for (j in 1:n_treat) {     # treatment category
    
    ate_pv <- numeric(n_pv)
    var_pv <- numeric(n_pv)
    
    for (m in 1:n_pv) {      # plausible value
      
      tau_forest <- list(
        models[[m]]$predictions_tau1,
        models[[m]]$predictions_tau2,
        models[[m]]$predictions_tau3,
        models[[m]]$predictions_tau4
      )
      
      tau <- tau_forest[[j]][, i, ]
      
      ate_draws <- apply(
        tau,
        2,
        function(x) sum(weights * x)
      )
      
      # posterior summaries within PV
      ate_pv[m] <- mean(ate_draws)
      var_pv[m] <- mean((ate_draws - ate_pv[m])^2)
    }
    
    # Rubin’s rules for SE
    ate_bar <- mean(ate_pv)
    W <- mean(var_pv)
    B <- var(ate_pv)
    T_var <- W + (1 + 1 / n_pv) * B
    se <- sqrt(T_var)
    
    ate_df$ATE[index] <- ate_bar
    ate_df$lower_quantile[index] <- ate_bar - 1.96 * se
    ate_df$upper_quantile[index] <- ate_bar + 1.96 * se
    
    ate_df$subject[index] <- ifelse(i == 1, "Math", "Science")
    
    ate_df$treatment[index] <- c(
      "Less than once a week",
      "1-2 times a week",
      "3-4 times a week",
      "5 times a week"
    )[j]
    
    index <- index + 1
  }
}

View(ate_df)


