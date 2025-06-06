library(nlme)
library(devtools)

load_all("GMJMCMC/R", compile=FALSE)

# Import necessary functions
source("do_simulation_error_ar1_nlme.R")
source("create_error_ar1_data_nlme.R")
source("Simulation-Studies---DS/helpers/measures.R")
source("Simulation-Studies---DS/helpers/cor_ops_funcs.R")

# Set random number generator: L'Ecuyer-CMRG is used to create reproducible parallel runs.
RNGkind("L'Ecuyer-CMRG")

# GMJMCMC parameters
transforms <- c("sin_deg", "sqroot", "p2")
ts_transforms <- c("lagged", "mov_avg")

probs <- gen.probs.gmjmcmc.ts(ts_transforms, transforms)
probs$gen <- c(1, 1, 0, 1, 1)

window_list <- list(1:3, c(10, 20))
lw <- max(unlist(window_list))

# Correlation structure options
cor_ops <- get_cor_ops_nlme()

# Data generation parameter: used to create the moving average feature.
ma <- 10

# Simulation parameters
n <- 101
n_pop <- 50
r_c <- 30
n_sims <- 20

# iid covariates sd
feat_sd <- 1

# AR(1) covariate parameters
ar_par <- 0.9
ar_tr <- 0.1
ar_sd <- 1
ar_pars <- c(ar_par, ar_sd, ar_tr)

error_noises <- c(0.01, 0.1, 1, 5, 10, 100)
for (i in 1:length(error_noises)) {
  error_sd <- error_noises[i]
  print(error_sd)
  fil_nam <- paste(error_sd, "_error_ar1_nlme_simul.txt", sep="")
  
  data <- create_error_ar1_data_nlme(n=n+lw+ma, ar_par = ar_par, ar_trend = ar_tr, ar_sd = ar_sd, 
                                    ma=ma, sd = feat_sd, noise_sd = error_sd, train_prop = 0)
  
  train_data <- data$train
  pred_data <- data$test
  
  # Data for the correlation structures
  n.row <- nrow(data$train)
  ind <- 1:n.row
  re_data_nlme <- as.data.frame(ind, nrow = n.row, ncol = 1)
  
  params <- gen.params.gmjmcmc.ts(train_data)
  params$max_rand_effects <- 3
  
  # True feature strings
  f_strings <- list(c("mov_avg(10, 1*x)"),
                    c("lagged(3, 1*x2)"),
                    c("x3"),
                    c("p2(1*lagged(2, 1*x4))", "lagged(2, 1*p2(1*x4))",
                      "lagged(2, 1*(x4*x4))", "(lagged(2, 1*x4)*lagged(2, 1*x4))"),
                    c("corARMA(form = ~ind, value=0.9, p=1, q=0, fixed=TRUE)"))

  simulation_error_ar1_nlme(data = train_data, pred_data = pred_data, filename = fil_nam,
                feature_strings = f_strings, re_data_nlme = re_data_nlme, re_ops = cor_ops, re_params = ar_pars,
                loglik = mixed.model.loglik.nlme, marg_lik_method = "nlme",
                transforms = transforms, ts_transforms = ts_transforms, window_list = window_list, 
                params = params, probs = probs, P = n_pop, N.init = 100, N.final = 100, add_lagged_response = FALSE,
                threshold = 0.5, sims = n_sims, noise_sd = error_sd, runs = r_c, cores = r_c, parallel = TRUE, verbose = FALSE)
}