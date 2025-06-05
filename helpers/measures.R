get_detection_measures <- function(feature_strings, imp_f_strs) {
  sims <- length(imp_f_strs)
  
  n_feat <- length(feature_strings)
  feature_strings <- unlist(feature_strings)
  n_alt_feat <- length(feature_strings) - n_feat
  
  fdr <- rep(NA, sims)
  recall <- rep(NA, sims)
  precision <- rep(NA, sims)
  f1 <- rep(NA, sims)
  
  fps <- 0
  tps <- 0
  fns <- 0
  power <- 0
  
  fps_sim <- rep(NA, sims)
  tps_sim <- rep(NA, sims)
  fns_sim <- rep(NA, sims)
  power_sim <- rep(NA, sims)
  
  for (i in 1:sims) {
    imp_feat_strings <- imp_f_strs[[i]]
    
    # True positives
    tp_bool <- imp_feat_strings %in% feature_strings
    tp <- sum(tp_bool)
    tps <- tps + tp
    tps_sim[i] <- tp
    
    # False positives
    fp_bool <- !tp_bool
    fp <- sum(fp_bool)
    fps <- fps + fp
    fps_sim[i] <- fp
    
    # False negatives
    fn_bool <- !(feature_strings %in% imp_feat_strings)
    fn <- sum(fn_bool) - n_alt_feat
    fns <- fns + fn
    fns_sim[i] <- fn
    
    # Power
    pow <- ((fp + fn) == 0)
    power <- power + pow
    power_sim[i] <- pow
    
    # FDR
    fdr[i] <- fdr(tp, fp)
    
    # Recall
    recall[i] <- recall(tp, fn)
    
    # Precision
    precision[i] <- precision(tp, fp)
    
    # f1
    f1[i] <- f1(tp, fp, fn)
  }
  
  detect_pop <- list(power=power_sim, fps=fps_sim, tps=tps_sim, fns=fns_sim, fdr=fdr, recall=recall, precision=precision, f1=f1)
  detect_names <- c("Power", "False positives", "True positives", "False negatives", "False discovery rate", 
                    "Recall", "Precision", "F1-score")
  
  pow <- get_stats("Power", power_sim)
  fps_n <- get_stats("False positives", fps_sim)
  tps_n <- get_stats("True positives", tps_sim)
  fns_n <- get_stats("False negatives", fns_sim)
  fdr <- get_stats("False discovery rate", fdr)
  recall <- get_stats("Recall", recall)
  precision <- get_stats("Precision", precision)
  f1 <- get_stats("F1-score", f1)
  detect_stats <- c(pow, fps_n, tps_n, fns_n, fdr, recall, precision, f1)
  
  return(list(detect_stats=detect_stats, detect_pop=detect_pop, detect_names=detect_names))
}

get_predictive_measures <- function(y_true, preds_mean_list) {
  sims <- length(preds_mean_list)
  mse <- rep(NA, sims)
  mae <- rep(NA, sims)
  mape <- rep(NA, sims)
  smape <- rep(NA, sims)
  r2 <- rep(NA, sims)
  
  cor_pear <- rep(NA, sims)
  cor_spearm <- rep(NA, sims)
  cor_kend <- rep(NA, sims)
  for (i in 1:sims) {
    preds_mean <- preds_mean_list[[i]]
    
    # Mean squared error
    mse[i] <- mse(y_true, preds_mean)
    
    # Mean absolute error
    mae[i] <- mae(y_true, preds_mean)
    
    # Mean absolute percentage error
    mape[i] <- mape(y_true, preds_mean)
    
    # Symmetric mean absolute percentage error
    smape[i] <- smape(y_true, preds_mean)
    
    # Correlations
    cor_pear[i] <- cor(y_true, preds_mean, method = "pearson")
    cor_spearm[i] <- cor(y_true, preds_mean, method = "spearman")
    cor_kend[i] <- cor(y_true, preds_mean, method = "kendall")
    
    # R^2 (Coefficient of determination)
    r2[i] <- r_squared(y_true, preds_mean)
  }
  pred_pop <- list(mse=mse, mae=mae, mape=mape, smape=smape, fdr=fdr, 
                   cor_pear=cor_pear, cor_spearm=cor_spearm, cor_kend=cor_kend, r2=r2)
  
  mse <- get_stats("Mean squared error", mse)
  mae <- get_stats("Mean absolute error", mae)
  mape <- get_stats("Mean absolute percentage error", mape)
  smape <- get_stats("Symmetric mean absolute percentage error", smape)
  r2 <- get_stats("R^2", r2)
  cor_pear <- get_stats("Pearson's correlation", cor_pear)
  cor_spearm <- get_stats("Spearman's correlation", cor_spearm)
  cor_kend <- get_stats("Kendall's correlation", cor_kend)
  pred_stats <- c(mse, mae, mape, smape, r2, cor_pear, cor_spearm, cor_kend)
  
  return(list(pred_stats=pred_stats, pred_pop=pred_pop))
}

update_detections <- function(imp_feat_strings, detections) {
  # Add detections for all features
  for (j in 1:length(imp_feat_strings)) {
    str <- imp_feat_strings[j]
    curr_detect <- detections[str]
    if (is.na(curr_detect)) {detections[str] <- 1
    } else detections[str] <- curr_detect + 1
  }
  return(detections)
}

format_detections <- function(d) {
  fd <- c("Detections:")
  feats <- names(d)
  for (i in 1:length(feats)) {
    feat <- feats[i]
    fd <- c(fd, paste(feat, ": ", d[feat], sep = ""))
  }
  return(fd)
}

get_stats <- function(header, stat) {
  head <- paste(header, "statistics:")
  mean <- paste("Mean:", mean(stat))
  sd <- paste("Standard deviation:", sd(stat))
  sorted <- sort(stat)
  best <- paste("Best:", sorted[1])
  worst <- paste("Worst:", sorted[length(sorted)])
  return(c(head, mean, sd, best, worst))
}

# Predictive measures
r_squared <- function(true, pred) {
  rss <- sum((true - pred)^2)
  mean_true <- mean(true)
  ss <- sum((true - mean_true)^2)
  r2 <- 1 - rss/ss
  return(r2)
}

# MSE
mse <- function(true, pred) {
  return(mean((true - pred)^2))
}

# MAE
mae <- function(true, pred) {
  return(mean(abs(true - pred)))
}

# MAPE
mape <- function(true, pred) {
  return(mean(abs((true - pred)/true)))
}

# sMAPE
smape <- function(true, pred) {
  num <- abs(true - pred)
  denom <- (abs(true) + abs(pred)) / 2
  return(mean(num/denom))
}

# FDR
fdr <- function(tp, fp) {
  return(fp/(fp+tp))
}

# Recall/Sensitivity/TPR
recall <- function(tp, fn) {
  return(tp/(tp+fn))
}

# Precision
precision <- function(tp, fp) {
  return(tp/(tp+fp))
}

# F1-score
f1 <- function(tp, fp, fn) {
  return(2 * tp / (2 * tp + fp + fn))
}

print_stats <- function(header, stat) {
  print(paste(header, "statistics:"))
  print(paste("Mean:", mean(stat)))
  print(paste("Standard deviation:", sd(stat)))
}