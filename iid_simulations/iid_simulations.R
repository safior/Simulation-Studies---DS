library(devtools)

load_all("GMJMCMC/R", compile=FALSE) # Path to GMJMCMC extension

# Import necessary functions
source("Simulation-Studies---DS/helpers/measures.R")
source("do_simulation_iid.R")
source("create_iid_simple.R")

# Set random number generator: L'Ecuyer-CMRG is used to create reproducible parallel runs.
RNGkind("L'Ecuyer-CMRG")

# GMJMCMC parameters
transforms <- c("sin", "sqroot", "p2")
ts_transforms <- c("lagged", "mov_avg")

probs <- gen.probs.gmjmcmc.ts(ts_transforms, transforms)
probs$gen <- c(1, 1, 0, 1, 1)

window_list <- list(1:3, c(10, 20))
lw <- max(unlist(window_list))

# Data generation parameter: used to create the moving average feature.
ma <- 10

# Simulation parameters
n <- 1001
n_pop <- 50
n_sims <- 20
r_c <- 30

# sd for iid covariates
feat_sd <- 1

# Parameters for autoregressive covariates
ar_par <- 0.9
ar_tr <- 0.1
ar_sd <- 1

noises <- c(0.01, 0.1, 1, 5, 10, 100)
for (i in 1:length(noises)) {
  noise_sd <- noises[i]
  print(noise_sd)
  fil_nam <- paste(noise_sd, "_iid_simple_simul.txt", sep="")
  
  data <- create_iid_simple(n=n+lw+ma, ar_par = ar_par, ar_trend = ar_tr, ar_sd = ar_sd, 
                            ma=ma, sd = feat_sd, noise_sd = noise_sd, train_prop = 0)
  
  train_data <- data$train
  pred_data <- data$test
  
  params <- gen.params.gmjmcmc.ts(train_data)
  
  f_strings_easy <- list(c("mov_avg(10, 1*x)"), 
                         c("lagged(3, 1*x2)"), 
                         c("x3"), 
                         c("p2(1*lagged(2, 1*x4))", "lagged(2, 1*p2(1*x4))",  
                           "lagged(2, 1*(x4*x4))", "(lagged(2, 1*x4)*lagged(2, 1*x4))"))
  simulation_iid(data = train_data, pred_data = pred_data, filename = fil_nam,
             feature_strings = f_strings_easy,
             transforms = transforms, ts_transforms = ts_transforms, window_list = window_list, 
             params = params, probs = probs, P = n_pop, N.init = 100, N.final = 100, add_lagged_response = FALSE,
             threshold = 0.5, sims = n_sims, noise_sd = noise_sd,
             runs = r_c, cores = r_c, parallel = TRUE, verbose = FALSE)
}


######################################
source("create_iid_complex.R")
noises <- c(0.01, 0.1, 1, 5, 10, 100)
for (i in 1:length(noises)) {
  noise_sd <- noises[i]
  print(noise_sd)
  fil_nam <- paste(noise_sd, "_iid_complex_simul.txt", sep="")

  data <- create_iid_complex(n=n+lw+ma, ar_par = ar_par, ar_trend = ar_tr, ar_sd = ar_sd, 
                              ma=ma, sd = feat_sd, noise_sd = noise_sd, train_prop = 0)

  train_data <- data$train
  pred_data <- data$test

  params <- gen.params.gmjmcmc.ts(train_data)

  f_strings_hard <- list(c("lagged_response"),
                           c("mov_avg(10, 1*x)"),
                           c("lagged(3, 1*x2)"),
                           c("(x3*lagged(3, 1*x2))", "(lagged(3, 1*x2)*x3)"),
                           c("p2(1*lagged(2, 1*x4))", "lagged(2, 1*p2(1*x4))",
                             "lagged(2, 1*(x4*x4))", "(lagged(2, 1*x4)*lagged(2, 1*x4))"),
                           c("sin(1*lagged(2, 1*x5))", "lagged(2, 1*sin(1*x5))"),
                           c("lagged(1, 1*x6)"))
  simulation_iid(data = train_data, pred_data = pred_data, filename = fil_nam,
             feature_strings = f_strings_hard,
             transforms = transforms, ts_transforms = ts_transforms, window_list = window_list,
             params = params, probs = probs, P = n_pop, N.init = 100, N.final = 100, add_lagged_response = TRUE,
             threshold = 0.5, sims = n_sims, noise_sd = noise_sd, runs = r_c, cores = r_c, parallel = TRUE, verbose = FALSE)
}
