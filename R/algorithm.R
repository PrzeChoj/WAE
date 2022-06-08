source("R/utils.R")

#' W mutacji liczba transpozycji będzie losowana z rozkladu binomialnego
#' 
#' @param a Współczynnik sterowania mutacją. O tyle przesuniemy się w prawo/lewo w funkcji sigmoid
#' 
#' @examples
#' my_goal_function <- goal_function_maker(10, 20)
#' out <- evolutional_optimization(my_goal_function, max_iter=100, pop_size=15)
#' out[["best_permutation"]]
#' out[["best_f_value"]]
#' out2 <- evolutional_optimization(my_goal_function, max_iter=100, pop_size=15, a=1)
evolutional_optimization <- function(my_goal_function, max_iter=100, pop_size=15,
                                     success_treshold=0.025, p_0=0.5,
                                     a=1, k_max=1,
                                     tournament_size=2,
                                     show_progress_bar=TRUE){
  if(show_progress_bar)
    progressBar <- utils::txtProgressBar(min = 0, max = max_iter, initial = 1)
  
  stopifnot(pop_size >= tournament_size,
            p_0 <= 1,
            p_0 >= 0,
            a >= 0)
  perm_size <- dim(attr(my_goal_function, "U"))[1]
  
  p_t_list <- numeric(max_iter)
  p_t_list[TRUE] <- NA
  p_t_list[1] <- p_0
  mutation_scaler <- log(p_0/(1-p_0)) # p_0 == sigm(mutation_scaler) == 1/(1+exp(-mutation_scaler))
  
  success_rate_list <- numeric(max_iter)
  success_rate_list[TRUE] <- NA
  
  
  mean_f_value_list <- numeric(max_iter)
  mean_f_value_list[TRUE] <- NA
  
  f_values <- numeric(pop_size)
  
  # init population
  population <- list()
  # TODO(start with an identity)
  #population[[1]] <- permutations::id # start with an identity
  #f_values[1] <- my_goal_function(population[[1]])
  for(i in 1:pop_size){
    population[[i]] <- runif_perm(perm_size)
    f_values[i] <- my_goal_function(population[[i]])
  }
  best_f_value_list <- max(f_values)
  names(best_f_value_list) <- "1"
  best_f_value <- best_f_value_list[1]
  best_permutation <- population[[which(f_values == best_f_value_list)[1]]]
  mean_f_value_list[1] <- mean(f_values)
  
  # mail loop
  for(iteration in 2:max_iter){
    if(show_progress_bar)
      utils::setTxtProgressBar(progressBar, iteration)
    
    # TODO(Reprodukcja)
    last_population <- population
    last_f_values <- f_values
    
    reproduced <- population
    reproduced_f_value <- f_values
    # end of TODO
    
    # mutation
    mutants <- list()
    mutants_f_values <- numeric(pop_size)
    
      # draw number of mutations for a specimens
    num_of_mutations <- pmax(rbinom(pop_size, k_max, p_t_list[iteration-1]), 1)
    for(i in 1:pop_size){
      mutation_perm <- permutations::id
      for(j in 1:num_of_mutations[i]){
        mutation_perm <- as.cycle(mutation_perm * runif_transposition(perm_size))  # TODO(this is computationally expensive)
      }
      
      mutants[[i]] <- as.cycle(reproduced[[i]] * mutation_perm)
      mutants_f_values[i] <- my_goal_function(mutants[[i]])
    }
    
    # modify p_t
    success_rate_list[iteration] <- mean(mutants_f_values > reproduced_f_value)
    if(success_rate_list[iteration] > success_treshold){
      mutation_scaler <- mutation_scaler + a # p_t will be bigger
    }else{
      mutation_scaler <- mutation_scaler - a # p_t will be smaller
    }
    p_t_list[iteration] <- 1 / (1 + exp(-mutation_scaler))
    
    # save the best mutant
    best_f_value_mutant <- max(mutants_f_values)
    num_of_best_values <- length(best_f_value_list)
    
    # new best?
    if(best_f_value_mutant > best_f_value_list[num_of_best_values]){
      best_f_value_list[num_of_best_values+1] <- best_f_value_mutant
      names(best_f_value_list) <- c(names(best_f_value_list)[1:num_of_best_values],
                                    as.character(iteration))
      best_f_value <- best_f_value_mutant
      best_permutation <- mutants[[which(mutants_f_values == best_f_value_mutant)[1]]]
    }
    
    
    # succession
    population <- list()
    
    # TODO(Elitaryzm)
    #stopifnot(pop_size > 10)
    #best_part <- order(f_values)[1:2]
    #for(i in 1:2){  # TODO(10 should be a parameter)
    #  population[[i]] <- last_population[[best_part[i]]]
    #  f_values[i] <- last_f_values[i]
    #}
    succession_type <- "tournament"
    if(succession_type == "tournament"){
      for(i in 1:pop_size){
        players <- sample(pop_size, tournament_size, replace = TRUE)
        
        tournament_f_values <- mutants_f_values[players]
        winner <- players[which(tournament_f_values == max(tournament_f_values))[1]]
        
        # TODO(ask if all specimens should be mutated)
        population[[i]] <- mutants[[winner]]
        f_values[i] <- mutants_f_values[winner]
      }
    }
    if(succession_type == "threshold"){
      mutants_bests <- order(mutants_f_values,
                             decreasing = TRUE)[1:ceiling(pop_size * 0.2)]
      
      for(i in 1:pop_size){
        player <- sample(mutants_bests, 1)
        
        population[[i]] <- mutants[[player]]
        f_values[i] <- mutants_f_values[player]
      }
    }
    
    # save the mean
    mean_f_value_list[iteration] <- mean(f_values)
  }
  
  names(mean_f_value_list) <- as.character(1:length(mean_f_value_list))
  
  if(show_progress_bar)
    close(progressBar)
  
  list("best_f_value_list" = best_f_value_list,
       "best_permutation" = best_permutation,
       "best_f_value" = best_f_value,
       "success_rate_list" = success_rate_list,
       "p_t_list" = p_t_list,
       "mean_f_value_list" = mean_f_value_list,
       "iterations_performed" = iteration)
}



monte_carlo <- function(my_goal_function, max_iter=100,
                        show_progress_bar=TRUE){
  if(show_progress_bar)
    progressBar <- utils::txtProgressBar(min = 0, max = max_iter, initial = 1)
  
  perm_size <- dim(attr(my_goal_function, "U"))[1]
  f_values <- numeric(0)
  
  f_values[1] <- my_goal_function(as.cycle(rperm(1, perm_size)))
  best_f_value_list <- f_values[1]
  names(best_f_value_list) <- "1"
  best_f_value <- best_f_value_list[1]
  num_of_best_values <- 1
  
  # mail loop
  for(iteration in 2:max_iter){
    if(show_progress_bar)
      utils::setTxtProgressBar(progressBar, iteration)
    
    new_permutation <- as.cycle(rperm(1, perm_size))
    f_values[iteration] <- my_goal_function(new_permutation)
    
    if(f_values[iteration] > best_f_value){
      best_permutation <- new_permutation
      best_f_value <- f_values[iteration]
      best_f_value_list[num_of_best_values+1] <- best_f_value
      names(best_f_value_list) <- c(names(best_f_value_list)[1:num_of_best_values],
                                    as.character(iteration))
      num_of_best_values <- num_of_best_values + 1
    }
  }
  
  list("best_f_value_list" = best_f_value_list,
       "best_permutation" = best_permutation,
       "best_f_value" = best_f_value,
       "iterations_performed" = iteration)
}




