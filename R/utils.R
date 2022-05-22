library(permutations)
library(gips)  # devtools::install_github("PrzeChoj/gips")

sigma_maker <- function(p){
  sigma <- matrix(numeric(p*p), nrow=p)
  for(i in 1:p){
    for(j in 1:p){
      sigma[i,j] <- 1 - min(abs(i-j), p-abs(i-j)) / p
    }
    sigma[i,i] <- 1 + 1/p
  }
  
  sigma
}

U_maker <- function(p, n, sigma=NULL){
  mu <- numeric(p)
  if(is.null(sigma)){
    sigma <- sigma_maker(p)
  }
  
  Z <- MASS::mvrnorm(n, mu = mu, Sigma = sigma)
  U <- t(Z) %*% Z
  
  U
}

goal_function_maker <- function(p, n, sigma=NULL){
  U <- U_maker(p, n, sigma)
  
  my_goal_function <- function(perm){
    goal_function(perm, n, U)
  }
  
  attr(my_goal_function, "U") <- U
  
  my_goal_function
}


#' Draw a random permutation
runif_perm <- function(perm_size){
  permutations::as.cycle(permutations::as.word(sample(perm_size, perm_size,
                                                      replace=FALSE)))
}

#' Drawing a random transosition
runif_transposition <- function(perm_size){
  permutations::as.cycle(sample(perm_size, 2, replace=FALSE))
}




