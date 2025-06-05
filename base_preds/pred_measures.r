pred_measures <- function(y_true, predictions, filename) {
  predictive_measures <- get_predictive_measures(y_true, predictions)
  pred_stats <- predictive_measures$pred_stats
  pred_pop <- predictive_measures$pred_pop

  lines <- c()
  
  sim_lines <- c()
  sim_lines <- c(sim_lines, paste("Predictive measures: ", sep=""))
  sim_lines <- c(sim_lines, paste("MSE: ", pred_pop$mse[1], sep=""))
  sim_lines <- c(sim_lines, paste("MAE: ", pred_pop$mae[1], sep=""))
  sim_lines <- c(sim_lines, paste("MAPE: ", pred_pop$mape[1], sep=""))
  sim_lines <- c(sim_lines, paste("SMAPE: ", pred_pop$smape[1], sep=""))
  sim_lines <- c(sim_lines, paste("Pearson: ", pred_pop$cor_pear[1], sep=""))
  sim_lines <- c(sim_lines, paste("Spearman: ", pred_pop$cor_spearm[1], sep=""))
  sim_lines <- c(sim_lines, paste("Kendall: ", pred_pop$cor_kend[1], sep=""))
  sim_lines <- c(sim_lines, paste("R2: ", pred_pop$r2[1], sep=""))
  
  lines <- c(lines, sim_lines)
  
  fileConn <- file(filename)
  writeLines(lines, fileConn)
  close(fileConn)
}