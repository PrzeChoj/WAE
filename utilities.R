goal_function_maker <- function(p, n, sigma=NULL){
  mu <- numeric(p)
  if(is.null(sigma)){
    sigma <- matrix(numeric(p*p), nrow=p)
    for(i in 1:p){
      for(j in 1:p){
        sigma[i,j] <- 1 - min(abs(i-j), p-abs(i-j)) / p
      }
      sigma[i,i] <- 1 + 1/p
    }
  }
  
  Z <- MASS::mvrnorm(n, mu = mu, Sigma = sigma)
  U <- t(Z) %*% Z
  
  my_goal_function <- function(perm){
    goal_function(perm, p, n, U)
  }
  
  attr(my_goal_function, "U") <- U
  
  my_goal_function
}

