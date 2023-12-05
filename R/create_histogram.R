# cutoff_percentile_to_position(cutoff_percentile)
cutoff_percentile_to_position <- function(cutoff_percentile){
  1+cutoff_percentile/5
}

create_histogram <- function(
    full_matrix,
    MNAR_threshold,
    MAR_prob,
    MNAR_prob,
    MAR,
    MNAR,
    transform_log2,
    breaks
){
  
  # ...missing values ----
  # creating missing values at random for both MAR and MNAR with different probabilities
  # choosing between MAR and MNAR is based on quantiles
  quantiles <- function(x) quantile(x, probs = c(seq(0,1,0.05)))
  MNAR_cutoff_values <- quantiles(full_matrix)
  cutoff_percentile <- MNAR_threshold
  MAR_probablity <- MAR_prob
  if (!MAR) MAR_probablity <- 0
  MNAR_probability <- MNAR_prob-0.2
  
  NA_matrix <- full_matrix
  MAR_values <- sample(length(full_matrix), length(full_matrix)*MAR_probablity, replace = F)
  NA_matrix[MAR_values] <- NA
  
  for (i in 1:nrow(NA_matrix)) {
    # print(i)
    tmp_mean <- mean(NA_matrix[i,], na.rm = T)
    if (tmp_mean < MNAR_cutoff_values[cutoff_percentile_to_position(cutoff_percentile)] & MNAR) {
      MNAR_values <- sample(
        ncol(NA_matrix),
        ncol(NA_matrix)*(MNAR_probability+(1-mean(NA_matrix[i,], na.rm = T)/MNAR_cutoff_values[cutoff_percentile_to_position(cutoff_percentile)])/2),
        replace = F
      )
      NA_matrix[i,MNAR_values] <- NA
    }
  }
  
  # ...generating the histograms ----
  
  # differences due to transformation
  if (transform_log2) {
    matrix_transformation_fun <- log2 # log2 transform
    text_to_title <- 'log transformed'
    xlab_text <- "log2(Intensity)"
    xlim_range <- c(15,35)
  }else{
    matrix_transformation_fun <- function(x) x # no change
    text_to_title <- 'the'
    xlab_text <- "Intensity"
    xlim_range <- c(0,5e+09)
  }
  
  hist1 <- hist(matrix_transformation_fun(full_matrix), breaks = breaks, plot = F)
  hist2 <- hist(matrix_transformation_fun(NA_matrix), breaks = breaks, plot = F)
  
  c1 <- rgb(173,216,230, max = 255, alpha = 80, names = "lt.blue")
  c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
  plot(hist1, col = c1, main = sprintf("Histogram of %s data", text_to_title), 
       xlab = xlab_text, xlim = xlim_range)
  plot(hist2, col = c2, add = T)
  
}