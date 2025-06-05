create_latent_ar1_data_inla <- function(n, ma, sd, noise_sd, ar_trend, ar_par, ar_sd, train_prop=0, seed=100){
    set.seed(seed)

    # Feature 1
    x <- rnorm(n, 1, sd)
    feature1 <- x
  
    # Feature 2
    x2 <- rep(0, n)
    for (i in 2:n) {
        x2[i] <- ar_trend + ar_par * x2[i-1] + rnorm(1, 0, ar_sd)
    }
    feature2 <- x2
    
    # Feature 3
    x3 <- rnorm(n, 0, sd)
    feature3 <- x3
    
    # Feature 4
    x4 <- rep(0, n)
    for (i in 2:n) {
        x4[i] <- ar_trend + ar_par * x4[i-1] + rnorm(1, 0, ar_sd)
    }
    feature4 <- x4

    ma_1 <- ma + 1
    
    # Latent variable
    ar1 <- rep(0, n)
    for (i in ma_1:n) {
        ar1[i] <- ar_par * ar1[i-1] + rnorm(1, 0, ar_sd)
    }

    y <- rep(NA, n)
    y[ma_1] <- 0 + feature1[ma_1] + feature2[ma_1] + feature3[ma_1] + feature4[ma_1] + ar1[ma_1]
    for (i in (ma_1+1):n) {
        y[i] <- feature1[i] + feature2[i] + feature3[i] + feature4[i] + ar1[i]
    }

    # Observational noise
    y <- y + rnorm(length(y), mean=0, sd=noise_sd)
    
    random1 <- rnorm(n, 0, sd)
    random2 <- rep(0, n)
    for (i in 2:n) {
        random2[i] <- ar_trend + ar_par * random2[i-1] + rnorm(1, 0, ar_sd)
    }

    data <- as.data.frame(cbind(y, x, x2, x3, x4, random1, random2))
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

    return(list(train=train_data, test=test_data))
}