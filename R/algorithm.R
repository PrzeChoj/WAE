source("R/utils.R")

#' W mutacji liczba transpozycji będzie losowana z rozkladu binomialnego
#' 
#' @examples
#' n_number <- 20
#' U <- U_maker(p=10, n=n_number)
#' out <- evolutional_optimization(U, n_number, max_iter=100, pop_size=5)
#' out[["best_permutation"]]
#' out[["best_f_value"]]
#' out <- evolutional_optimization(U, n_number, max_iter=100, pop_size=3, a=1)
evolutional_optimization <- function(U, n_number, max_iter=100, pop_size=3,
                                     success_treshold=0.2, p_0=0.5,
                                     a=0.817, k_max=1,
                                     tournament_size=2,
                                     delta=3, D_matrix=NULL,
                                     show_progress_bar=TRUE){
  if(show_progress_bar)
    progressBar <- utils::txtProgressBar(min = 0, max = max_iter, initial = 1)
  
  stopifnot(dim(U)[1] == dim(U)[2])
  perm_size <- dim(U)[1]
  
  if(is.null(D_matrix)){
    D_matrix <- diag(nrow = perm_size)
  }
  
  p_t <- p_0
  
  my_goal_function <- function(perm){
    gips::goal_function(perm, perm_size, n_number, U,
                        delta=delta, D_matrix=D_matrix)
  }
  
  f_values <- numeric(pop_size)
  best_f_value_list <- 0  # f is always positive, so this is smaller than any f value
  best_permutation <- NULL
  
  # init population
  population <- list()
  for(i in 1:pop_size){
    population[[i]] <-  runif_perm(perm_size)
    f_values[i] <- my_goal_function(population[[i]])
  }
  best_f_value_list <- max(f_values)
  names(best_f_value_list) <- "1"
  best_f_value <- best_f_value_list[1]
  best_permutation <- population[[which(f_values == best_f_value_list)[1]]]
  
  # mail loop
  for(iteration in 2:max_iter){
    if(show_progress_bar)
      utils::setTxtProgressBar(progressBar, iteration)
    
    # TODO(iteracja)
    
    # TODO(Reprodukcja)
    reproduced <- population
    reproduced_f_value <- f_values
    # end of TODO
    
    # mutation
    mutants <- list()
    mutants_f_values <- numeric(pop_size)
    
      # draw number of mutations for a specimens
    num_of_mutations <- pmax(rbinom(pop_size, k_max, p_t), 1)
    for(i in 1:pop_size){
      mutation_perm <- permutations::id
      for(j in 1:num_of_mutations[i]){
        mutation_perm <- as.cycle(mutation_perm * runif_transposition(perm_size))
      }
      
      mutants[[i]] <- as.cycle(reproduced[[i]] * mutation_perm)
      mutants_f_values[i] <- my_goal_function(mutants[[i]])
    }
    
    success_rate <- mean(mutants_f_values > reproduced_f_value)
    
    if(success_rate > success_treshold){
      p_t <- 1 - (1 - p_t) * a # p_t will be bigger
    }else{
      p_t <- p_t * a # p_t will be smaller
    }
    
    # TODO succession
    population <- mutants
    f_values <- mutants_f_values
    # end of TODO
    
    # save the best
    best_f_value_mutant <- max(mutants_f_values)
    num_of_best_values <- length(best_f_value_list)
    
    if(best_f_value_mutant > best_f_value_list[num_of_best_values]){
      best_f_value_list[num_of_best_values+1] <- best_f_value_mutant
      names(best_f_value_list) <- c(names(best_f_value_list)[1:num_of_best_values],
                                    as.character(iteration))
      best_f_value <- best_f_value_mutant
      best_permutation <- mutants[[which(mutants_f_values == best_f_value_mutant)[1]]]
    }
  }
  
  if(show_progress_bar)
    close(progressBar)
  
  list("best_f_value_list" = best_f_value_list,
       "best_permutation" = best_permutation,
       "best_f_value" = best_f_value)
}
























