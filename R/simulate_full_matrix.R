simulate_full_matrix <- function(
    matrix_ncol=20,
    matrix_nrow=1000,
    number_of_proteins=1000,
    number_of_G_samples=2,
    number_of_samples_per_G=10,
    number_of_diff=50
){
  
  full_matrix <- matrix(rep(1, matrix_nrow*matrix_ncol), ncol = matrix_ncol)
  
  test_samples <- sapply(seq(number_of_G_samples), function(g){
    paste0(seq(number_of_samples_per_G), "_G", g)
  })
  
  number_of_samples <- length(test_samples)
  
  # mnar and mar generation
  
  # two sample groups are calculated separately
  
  # data should lognormal distribution
  # calculating protein means
  protein_mean <- rlnorm(number_of_proteins, meanlog = 17, sdlog = 1.5)
  
  # stdev is intensity-dependent, much higher for lower abundances
  protein_stdev <- rlnorm(number_of_proteins, mean = -1.5, sd = 0.5)
  # hist(protein_stdev)
  
  for (i in 1:number_of_proteins) {
    original_test_values <- rnorm(number_of_samples,
                                  mean = protein_mean[i],
                                  sd = protein_mean[i]*protein_stdev[i])
    
    transformed_test_values <- abs(original_test_values)
    
    # I use abs because a few values are negative
    full_matrix[i,] <- t(transformed_test_values)
    
  }
  
  # assigning set number of proteins with differential expression at random
  # the FC is also randomized according to a normal distribution
  changing_proteins <- sample(number_of_proteins, number_of_diff, replace = F)
  
  for (x in changing_proteins) {
    
    if (random_coin_toss()) {
      grow_range <- 1:10
      grow_multiply <- random_coin_toss()
    } else {
      grow_range <- 11:20
      grow_multiply <- random_coin_toss()
    }
    
    grow_rnorm <- rnorm(1, mean = 4, sd = 1)
    
    if(grow_multiply){
      full_matrix[x, grow_range] <- full_matrix[x, grow_range] * grow_rnorm
    }else{
      full_matrix[x, grow_range] <- full_matrix[x, grow_range] / grow_rnorm
    }
    
  }
  
  full_matrix
}