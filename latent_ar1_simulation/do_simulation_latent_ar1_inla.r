simulation_latent_ar1_inla <- function(data, pred_data, filename, feature_strings, re_data, re_ops, re_params,
                          loglik = mixed.model.loglik.inla, marg_lik_method,
                          transforms, ts_transforms, window_list, params, probs, P, N.init, N.final, add_lagged_response,
                          noise_sd, sims, runs, cores, 
                          threshold = 0.5, seed = 100, parallel = FALSE, verbose = FALSE) {

  RNGkind("L'Ecuyer-CMRG")
  
  feat_strings <- unlist(feature_strings)
  n_feat <- length(feat_strings)
  detections <- NULL
  for (i in 1:n_feat) {
    detections[feat_strings[i]] <- 0
  }
  
  if (add_lagged_response){
    var_names <- c(names(data)[-1], "lagged_response")
  }
  else {
    var_names <- names(data)[-1]
  }
  
  tot_run_time <- 0
  run_time <- rep(NA, sims)
  
  imp_f_strs <- list()
  predictions <- list()
  for (i in 1:sims) {
    start_time <- Sys.time()
    set.seed(seed+i)
    if(parallel) {
      re_par <- gmjmcmc.parallel.re(runs = runs, cores = cores,
                                    data_ts = data,
                                    transforms = transforms,
                                    loglik.pi = loglik,
                                    ts_transforms = ts_transforms,
                                    probs = probs,
                                    window_list = window_list,
                                    params = params,
                                    P = P,
                                    N.init = N.init,
                                    N.final = N.final,
                                    add_lagged_response = add_lagged_response,
                                    re_data = re_data,
                                    rand_effects = re_ops,
                                    marg_lik_method = marg_lik_method,
                                    verbose = verbose)
      
      preds <- predict.gmjmcmc.inla(re_par, data)  
      feat_df <- summary.gmjmcmc_merged.re(re_par, labels = var_names)
    }
    else{
      re_par <- gmjmcmc.re2(
        data_ts = data,
        transforms = transforms,
        loglik.pi = loglik,
        ts_transforms = ts_transforms,
        probs = probs,
        window_list = window_list,
        params = params,
        P = P,
        N.init = N.init,
        N.final = N.final,
        add_lagged_response = add_lagged_response,
        re_data = re_data,
        rand_effects = re_ops,
        marg_lik_method = marg_lik_method,
        verbose = verbose)
      
      preds <- predict.gmjmcmc.inla(re_par, data)
      feat_df <- summary.gmjmcmc.re(re_par, labels = var_names)
    }
    
    # Features above threshold
    imp_feat <- feat_df[feat_df[, 2] > threshold, ]
    imp_feat_strings <- imp_feat[, 1]
    if (length(imp_feat_strings)==0) {
      imp_feat_strings <- c("No features above threshold")
    }
    
    detections <- update_detections(imp_feat_strings, detections)
    
    # Feaures above threshold
    imp_f_strs[[i]] <- imp_feat_strings
    
    # Predictions
    predictions[[i]] <- preds$aggr$mean
    
    # Time
    end_time <- Sys.time()
    time <- end_time - start_time
    run_time[i] <- time
    tot_run_time <- tot_run_time + time
  }
  
  lw <- max(unlist(window_list))
  if (add_lagged_response) {
    start_ind <- lw + 2
  }
  else {
    start_ind <- lw + 1
  }
  y_true <- pred_data[start_ind:nrow(pred_data), 1]
  
  tot_run_time <- paste("Total run time: ", tot_run_time)
  
  # Parameters
  ar_par <- paste("ar_par:", re_params[1])
  ar_sd <- paste("ar_sd:", re_params[2])
  ar_tr <- paste("ar_tr:", re_params[3])
  n.ini <- paste("N.init: ", N.init)
  n.fin <- paste("N.final: ", N.final)
  pops <- paste("Populations: ", P)
  noise <- paste("Noise SD: ", noise_sd)
  thres <- paste("Threshold: ", threshold)
  simus <- paste("Simulations: ", sims)
  run_parallel <- paste("Parallel: ", parallel)
  rs <- paste("Runs: ", runs)
  cs <- paste("Cores: ", cores)
  marg_meth <- paste("Marginal likelihood method: ", marg_lik_method)
  add_l_resp <- paste("Add lagged response: ", add_lagged_response)
  rand_sed <- paste("Seed: ", seed)
  
  space <- paste("\n#######################", sep="")
  params_vec <- c(tot_run_time, ar_par, ar_sd, ar_tr, n.ini, n.fin, pops, noise, thres, simus, thres, 
                  run_parallel, rs, cs, marg_meth, add_l_resp, rand_sed, space)
  
  detects <- format_detections(detections)
  
  # Get detection measure
  detection_measures <- get_detection_measures(feature_strings, imp_f_strs)
  detect_stats <- detection_measures$detect_stats
  detect_pop <- detection_measures$detect_pop
  detect_names <- detection_measures$detect_names
  
  # Get predictive measures
  predictive_measures <- get_predictive_measures(y_true, predictions)
  pred_stats <- predictive_measures$pred_stats
  pred_pop <- predictive_measures$pred_pop
  
  results_vec <- c(detect_stats, pred_stats, detects)
  
  lines <- c(params_vec, results_vec)
  
  sim_lines <- c()
  for (i in 1:sims) {
    sim_lines <- c(sim_lines, paste("\n######################", sep=""))
    sim_lines <- c(sim_lines, paste("Simulation: ", i, sep=""))
    sim_lines <- c(sim_lines, paste("Run time: ", run_time[i], sep=""))
    
    sim_lines <- c(sim_lines, paste("Detective measures:", sep=""))
    sim_lines <- c(sim_lines, paste("Power: ", detect_pop$power[i], sep=""))
    sim_lines <- c(sim_lines, paste("False positives ", detect_pop$fps[i], sep=""))
    sim_lines <- c(sim_lines, paste("True positives: ", detect_pop$tps[i], sep=""))
    sim_lines <- c(sim_lines, paste("False negatives: ", detect_pop$fns[i], sep=""))
    sim_lines <- c(sim_lines, paste("False discovery rate: ", detect_pop$fdr[i], sep=""))
    sim_lines <- c(sim_lines, paste("Recall: ", detect_pop$recall[i], sep=""))
    sim_lines <- c(sim_lines, paste("Precision: ", detect_pop$precision[i], sep=""))
    sim_lines <- c(sim_lines, paste("F1-score: ", detect_pop$f1[i], sep=""))
    
    sim_lines <- c(sim_lines, paste("Predictive measures: ", sep=""))
    sim_lines <- c(sim_lines, paste("MSE: ", pred_pop$mse[i], sep=""))
    sim_lines <- c(sim_lines, paste("MAE: ", pred_pop$mae[i], sep=""))
    sim_lines <- c(sim_lines, paste("MAPE: ", pred_pop$mape[i], sep=""))
    sim_lines <- c(sim_lines, paste("SMAPE: ", pred_pop$smape[i], sep=""))
    sim_lines <- c(sim_lines, paste("Pearson: ", pred_pop$cor_pear[i], sep=""))
    sim_lines <- c(sim_lines, paste("Spearman: ", pred_pop$cor_spearm[i], sep=""))
    sim_lines <- c(sim_lines, paste("Kendall: ", pred_pop$cor_kend[i], sep=""))
    sim_lines <- c(sim_lines, paste("R2: ", pred_pop$r2[i], sep=""))
    
    sim_lines <- c(sim_lines, paste("Features in simulation: ", sep=""))
    sim_lines <- c(sim_lines, imp_f_strs[[i]])
  }
  
  lines <- c(lines, sim_lines)
  
  fileConn <- file(filename)
  writeLines(lines, fileConn)
  close(fileConn)
}