create_iid_complex <- function(n, ma, sd, noise_sd, ar_trend, ar_par, ar_sd, train_prop=0, seed=100){
  set.seed(seed)
  
  # Feature 1
  x <- rnorm(n, 1, sd)
  feature1 <- mov_avg(ma, x)
  
  # Feature 2
  #x2 <- rnorm(n, 0, sd)
  x2 <- rep(0, n)
  for (i in 2:n) {
    x2[i] <- ar_trend + ar_par * x2[i-1] + rnorm(1, 0, ar_sd)
  }
  feature2 <- lagged(3, x2)
  
  # Feature 3
  x3 <- rnorm(n, 0, sd)
  feature3 <- x3 * feature2
  
  # Feature 4
  x4 <- rep(0, n)
  for (i in 2:n) {
    x4[i] <- ar_trend + ar_par * x4[i-1] + rnorm(1, 0, ar_sd)
  }
  #feature4 <- sin(lagged(2, x4))
  feature4 <- p2(lagged(2, x4))
  
  # noise, random1 and random2 is generated here to make the common covariates equal to easy simulation
  noise <- rnorm(n, mean=0, sd=noise_sd)
  
  random1 <- rnorm(n, 0, sd)
  random2 <- rep(0, n) #rnorm(n, 0, sd)
  for (i in 2:n) {
    random2[i] <- ar_trend + ar_par * random2[i-1] + rnorm(1, 0, ar_sd)
  }
  
  # Feature 5
  x5 <- rep(0, n)
  for (i in 2:n) {
    x5[i] <- ar_trend + ar_par * x5[i-1] + rnorm(1, 0, ar_sd)
  }
  #feature5 <- p2(lagged(2, x5))
  feature5 <- sin(lagged(2, x5))
  
  # Feature 6
  x6 <- rnorm(n, 0, sd)
  feature6 <- lagged(1, x6)
  
  ma_1 <- ma + 1
  y <- rep(NA, n)
  y[ma_1] <- 0 + feature1[ma_1] + feature2[ma_1] + feature3[ma_1] + feature4[ma_1] + feature5[ma_1] + feature6[ma_1]
  for (i in (ma_1+1):n) {
    y[i] <- 0.5*y[i-1] + feature1[i] +  feature2[i] + feature3[i] + feature4[i] + feature5[i] + feature6[i]
  }
  
  # Noise
  y <- y + noise
  
  
  data <- as.data.frame(cbind(y, x, x2, x3, x4, x5, x6, random1, random2))#, random3))
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