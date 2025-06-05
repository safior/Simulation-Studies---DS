library(INLA)
library(devtools)

load_all("GMJMCMC/R") # Path to GMJMCMC extension

# Import necessary functions
source("do_simulation_latent_ar1_inla.r")
source("create_latent_ar1_data_inla.r")
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
re_ops <- get_cor_ops_inla()

# Data generation parameter: used to create the moving average feature.
ma <- 10

# Simulation parameters
n <- 101
n_pop <- 1
r_c <- 1
n_sims <- 5

# iid covariates sd
feat_sd <- 1

# AR(1) covariate parameters
ar_par <- 0.9
ar_tr <- 0.1
ar_sd <- 1
ar_pars <- c(ar_par, ar_sd, ar_tr)

noises <- c(0.01, 0.1, 1, 5, 10, 100)
for (i in 1:length(noises)) {
    noise_sd <- noises[i]
    fil_nam <- paste(noise_sd, "_latent_ar1_inla_simul.txt", sep="")

    data <- create_latent_ar1_data_inla(n=n+lw+ma, ar_par = ar_par, ar_trend = ar_tr, ar_sd = ar_sd, 
                                ma=ma, sd = feat_sd, noise_sd = noise_sd, train_prop = 0)

    train_data <- data$train
    pred_data <- data$test

    # Data for the correlation structures
    n.row <- nrow(data$train)
    ind <- 1:n.row
    re_data_inla <- as.data.frame(ind, nrow = n.row, ncol = 1)

    params <- gen.params.gmjmcmc.ts(train_data)
    params$max_rand_effects <- length(re_ops)

    # Not run in parallel
    par <- FALSE

    f_strings <- list(c("x"),
                        c("x2"),
                        c("x3"),
                        c("x4"),
                        c("f(ind, model = \"ar1\")"))
    simulation_latent_ar1_inla(data = train_data, pred_data = pred_data, filename = fil_nam,
                    feature_strings = f_strings, re_data = re_data_inla, re_ops = re_ops, re_params = ar_pars,
                    loglik = loglik.inla, marg_lik_method = "inla",
                    transforms = transforms, ts_transforms = ts_transforms, window_list = window_list, 
                    params = params, probs = probs, P = n_pop, N.init = 100, N.final = 100, add_lagged_response = FALSE,
                    threshold = 0.5, sims = n_sims, noise_sd = noise_sd, runs = r_c, cores = r_c, parallel = par, verbose = !par)
}