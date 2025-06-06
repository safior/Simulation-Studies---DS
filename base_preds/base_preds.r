source("experiments_nam-shub/experiments/data_gen/create_error_ar1_data_nlme.R")
source("experiments_nam-shub/experiments/data_gen/create_latent_ar1_data_inla.R")
source("experiments_nam-shub/experiments/data_gen/create_iid_simple.R")
source("experiments_nam-shub/experiments/data_gen/create_iid_complex.R")
source("experiments_nam-shub/pred_measures.r")
source("experiments_nam-shub/experiments/measures.R")

RNGkind("L'Ecuyer-CMRG")

# Parameters used to create the AR(1) covariates
feat_sd <- 1
ar_par <- 0.9
ar_tr <- 0.1
ar_sd <- 1
ar_pars <- c(ar_par, ar_sd, ar_tr)

# Max loockback option
window_list <- list(1:3, c(10, 20))
lw <- max(unlist(window_list))
start_ind <- lw+1

# ma denotes the number of points used to create the moving average feature. 
# ma is added to the n parameter below because the 10 first points are discarded in the data-generating process.
ma <- 10
n <- 101

error_noises <- c(0.01, 0.1, 1, 5, 10, 100)
for (i in 1:length(error_noises)) {
    error_sd <- error_noises[i]
    data <- create_latent_ar1_data_inla(n=n+lw+ma, ar_par = ar_par, ar_trend = ar_tr, ar_sd = ar_sd, 
                            ma=ma, sd = feat_sd, noise_sd = error_sd, train_prop = 0)
    data_t <- data$test
    y_true <- data_t[start_ind:nrow(data_t), 1]
    predictions <- data_t[(start_ind-1):(nrow(data_t)-1), 1]
    filename <- paste(error_sd, "_baseline_preds_latent_ar1_inla.txt")
    pred_measures(y_true, list(predictions), filename)
}

for (i in 1:length(error_noises)) {
  error_sd <- error_noises[i]
  data <- create_error_ar1_data_nlme(n=n+lw+ma, ar_par = ar_par, ar_trend = ar_tr, ar_sd = ar_sd, 
                               ma=ma, sd = feat_sd, noise_sd = error_sd, train_prop = 0)
  data <- data$train
  y_true <- data[start_ind:nrow(data), 1]
  predictions <- data[(start_ind-1):(nrow(data)-1), 1]
  filename <- paste(error_sd, "_baseline_preds_error_ar1_nlme.txt", sep="")
  pred_measures(y_true, list(predictions), filename)
}

# n <- 1001 # Tables in thesis created with below code were generated with n <- 101
for (i in 1:length(error_noises)) {
  noise_sd <- error_noises[i]
  data <- create_iid_simple(n=n+lw+ma, ar_par = ar_par, ar_trend = ar_tr, ar_sd = ar_sd, 
                            ma=ma, sd = feat_sd, noise_sd = noise_sd, train_prop = 0)
  
  data <- data$train
  y_true <- data[start_ind:nrow(data), 1]
  predictions <- data[(start_ind-1):(nrow(data)-1), 1]
  filename <- paste(noise_sd, "_baseline_preds_simple.txt", sep="")
  pred_measures(y_true, list(predictions), filename)
}

for (i in 1:length(error_noises)) {
  noise_sd <- error_noises[i]
  data <- create_iid_complex(n=n+lw+ma, ar_par = ar_par, ar_trend = ar_tr, ar_sd = ar_sd, 
                              ma=ma, sd = feat_sd, noise_sd = noise_sd, train_prop = 0)
  
  data <- data$train
  y_true <- data[start_ind:nrow(data), 1]
  predictions <- data[(start_ind-1):(nrow(data)-1), 1]
  filename <- paste(noise_sd, "_baseline_preds_iid_complex.txt", sep="")
  pred_measures(y_true, list(predictions), filename)
}