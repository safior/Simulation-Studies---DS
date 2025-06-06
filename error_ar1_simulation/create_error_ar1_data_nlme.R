create_error_ar1_data_nlme <- function(n, ma, sd, noise_sd, ar_trend, ar_par, ar_sd, train_prop=0, seed=100){
  set.seed(seed)
  
  # Feature 1
  x <- rnorm(n, 1, sd)
  feature1 <- mov_avg(ma, x)
  
  # Feature 2
  x2 <- rep(0, n)
  for (i in 2:n) {
    x2[i] <- ar_trend + ar_par * x2[i-1] + rnorm(1, 0, ar_sd)
  }
  feature2 <- lagged(3, x2)
  
  # Feature 3
  x3 <- rnorm(n, 0, sd)
  feature3 <- x3
  
  # Feature 4
  x4 <- rep(0, n)
  for (i in 2:n) {
    x4[i] <- ar_trend + ar_par * x4[i-1] + rnorm(1, 0, ar_sd)
  }
  feature4 <- p2(lagged(2, x4))
  
  ma_1 <- ma + 1
  
  # Error
  ar1 <- rep(0, n)
  for (i in ma_1:n) {
    ar1[i] <- ar_par * ar1[i-1] + rnorm(1, 0, noise_sd) #0.9 * ar1[i-1] + rnorm(1, 0, 0.1)
  }
  
  y <- rep(NA, n)
  y[ma_1] <- 0 + feature1[ma_1] + feature2[ma_1] + feature3[ma_1] + feature4[ma_1] + ar1[ma_1] #+ feature5[ma_1] #+ feature6[ma_1]
  for (i in (ma_1+1):n) {
    y[i] <- feature1[i] + feature2[i] + feature3[i] + feature4[i] + ar1[i]#+ feature5[i] #+ feature6[i] #+ 0.9 * y[i-1]
  }
  
  # Observational noise
  #y <- y + rnorm(length(y), mean=0, sd=noise_sd)
  
  random1 <- rnorm(n, 0, sd)
  random2 <- rep(0, n) #rnorm(n, 0, sd)
  for (i in 2:n) {
    random2[i] <- ar_trend + ar_par * random2[i-1] + rnorm(1, 0, ar_sd)
  }
  #random3 <- rnorm(n, 0, sd)
  data <- as.data.frame(cbind(y, x, x2, x3, x4, random1, random2))#, random3))
  data <- data[(ma_1+1):nrow(data), ]
  
  if (train_prop>0) {
    n_row <- nrow(data)
    train_ind <- ceiling(n_row*train_prop)
    train_inds <- 1:train_ind
    train_data <- data[train_inds, ]
    test_data <- data[-train_inds, ]
  } 
  else {
    train_data <- data
    test_data <- data
  }
  
  # Scale data
  #data <- scale(data, center = TRUE, scale = TRUE)
  
  return(list(train=train_data, test=test_data))
}