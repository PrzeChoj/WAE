source("utils.R")

#' W mutacji liczba transpozycji będzie losowana z rozkladu binomialnego
evolutional_optimization <- function(U, n_number, max_iter=100, pop_size=100,
                                     success_treshold=0.2, a=0.817,
                                     tournament_size=2,
                                     delta=3, D_matrix=NULL,
                                     use_progress_bar=TRUE){ # TODO(use_progress_bar)
  perm_size <- dim(U)[1]
  
  stopifnot(perm_size == dim(U)[1])
  if(is.null(D_matrix)){
    D_matrix <- diag(nrow = perm_size)
  }
  
  f_values <- numeric(pop_size)
  best_f_value <- 0  # f is always positive, so this is smaller than any f value
  best_permutation <- NULL
  
  population <- list()
  for(i in 1:pop_size){
    population[[i]] <-  runif_perm(perm_size)
    f_values[i] <- goal_function(population[[i]],
                                 perm_size, n_number, U,
                                 delta=delta, D_matrix=D_matrix)
    
    if(f_values[i] > best_f_value){
      best_f_value <- f_values[i]
      best_permutation <- population[[i]]
    }
  }
  
  for(iteration in 2:max_iter){
    # TODO
  }
  
  list("best_f_value" = best_f_value, "best_permutation" = best_permutation)
}
























