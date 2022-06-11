library(magrittr)
source("R/algorithm.R") # devtools::install_github("PrzeChoj/gips")

set.seed(1234)

perform_experiment <- "2"

n_number <- 20
if(perform_experiment == "1"){
  perm_size <- 25
  sigma_matrix <- NULL
  
  perm_real <- as.cycle(1:perm_size)
}else if(perform_experiment == "2"){
  perm_size <- 26
  
  # generate sigma matrix:
  org_sigma_matrix <- sigma_maker(13)
  sigma_matrix <- matrix(numeric(26*26), nrow = 26)
  
  for(i in 1:13){
    for(j in 1:13){
      sigma_matrix[i,j] <- org_sigma_matrix[i,j]
      sigma_matrix[i+13,j+13] <- org_sigma_matrix[i,j]
      sigma_matrix[i, j+13] <- - org_sigma_matrix[i,j] / 2
      sigma_matrix[i+13, j] <- - org_sigma_matrix[i,j] / 2
    }
  }
  # check properties:
  # eigen(sigma_matrix)$values # add positive; matrix is positive-defined; matrix can be used as CoV matrix
  
  perm_real <- as.cycle(as.word(c(2:13, 1, 15:26, 14)))
  
  # gips::project_matrix(my_sigma_matrix, perm_real, 26) - my_sigma_matrix # matrix of zeros; this matrix is invariant under perm_real
}else{
  stop("Wrong experiment selected!")
}

# prod(1:perm_size) / 100 / 60 / 60  # liczba godzin potrzebnych do przejrzenia calej dziedziny

my_goal_function <- goal_function_maker(perm_size, n_number, sigma = sigma_matrix)
U <- attr(my_goal_function, "U")

(f_val_max <- my_goal_function(perm_real)) # 1 --->>> 173.8; 2 --->>> 56.00
(f_val_id <- my_goal_function(permutations::id)) # 1 --->>> 79.5; 2 --->>> -108.26



# MH for reference
mh_list1e4 <- get_list_of_log_values_MH(my_goal_function, max_iter = 10000, M = 100) # PC 2 h
#save(mh_list1e4, file=paste0("data/experiment", perform_experiment, "/mh_list1e4.Rdata")) # CAUTIOUSLY! Not to overwrite!


# MH longer for reference
set.seed(1234)
mh_list1e5 <- get_list_of_log_values_MH(my_goal_function, max_iter = 100000, M = 10) # PC 2 h
#save(mh_list1e5, file=paste0("data/experiment", perform_experiment, "/mh_list1e5.Rdata")) # CAUTIOUSLY! Not to overwrite!



# MC for reference
set.seed(1234)
mc_list <- get_list_of_log_values_MC(my_goal_function, max_iter = 10000, M = 100) # PC 70 min
#save(mc_list, file=paste0("data/experiment", perform_experiment, "/mc_list.Rdata")) # CAUTIOUSLY! Not to overwrite!

f_val_min <- 1:length(mc_list) %>% sapply(function(i){min(mc_list[[i]])}) %>% min # smallest of drawn values
f_val_med <- median(mc_list[[1]]) # median of first part of drawn values


# BG for reference
set.seed(1234)
bg_start_id_list <- list(best_growth(attr(my_goal_function, "U"), n_number = attr(my_goal_function, "n"),
                                     max_iter = 20)[["goal_function_logvalues"]]) # max_iter == 14 is enough; bg_start_id is NOT random; PC 1 min
#save(bg_start_id_list, file=paste0("data/experiment", perform_experiment, "/bg_start_id_list.Rdata")) # CAUTIOUSLY! Not to overwrite!

set.seed(1234)
# Use this with caution!!! It is incredibly hard to interpret! Only part of the length of the line is sensible!
bg_start_random_list <- get_list_of_log_values_BG(my_goal_function, max_iter = 100, M = 100) # PC 27 min
#save(bg_start_random_list, file=paste0("data/experiment", perform_experiment, "/bg_start_random_list.Rdata")) # CAUTIOUSLY! Not to overwrite!
#bg_start_random_list_mean <- make_BG_mean(bg_start_random_list) # Use this with caution!!! It is incredibly hard to interpret! Only part of the length of the line is sensible!





# Tuning parameters

# 1. Start with small tuning for a:
set.seed(1234)
my_a <- c(0.1, 0.3, 0.5, 1)
eo_list_out_1 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.025, a = my_a,
                                                 k_max = 5, tournament_part = 0.5,
                                                 M = 10, max_iter = 100) # PC 70 min
#save(eo_list_out_1, file=paste0("data/experiment", perform_experiment, "/eo_list_out_1.Rdata")) # CAUTIOUSLY! Not to overwrite!



# 2. Start tuning for k_max:
set.seed(1234)
my_k_max <- c(1, 2, 3, 4, 7, 14, 20)
eo_list_out_2 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.025, a = 0.3,
                                                 k_max = my_k_max, tournament_part = 0.5,
                                                 M = 30, max_iter = 100) # PC 6 hours 20 minutes
#save(eo_list_out_2, file=paste0("data/experiment", perform_experiment, "/eo_list_out_2.Rdata")) # CAUTIOUSLY! Not to overwrite!



# 3. Start tuning for pop_size:
set.seed(1234)
my_pop_size <- c(10, 30, 70, 100, 150, 200)
eo_list_out_3 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = my_pop_size,
                                                 success_treshold = 0.025, a = 0.3,
                                                 k_max = 4, tournament_part = 0.5,
                                                 M = 10, max_iter = 1000, max_f_calls = 10000) # PC 2 h
#save(eo_list_out_3, file=paste0("data/experiment", perform_experiment, "/eo_list_out_3.Rdata")) # CAUTIOUSLY! Not to overwrite!



# 4. Start tuning for tournament_part:
set.seed(1234)
my_tournament_part <- c(0.07, 0.11, 0.2, 0.35, 0.5, 0.65)
eo_list_out_4 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.025, a = 0.3,
                                                 k_max = 4, tournament_part = my_tournament_part,
                                                 M = 15, max_iter = 1000, max_f_calls = 10000) # PC 3 h
#save(eo_list_out_4, file=paste0("data/experiment", perform_experiment, "/eo_list_out_4.Rdata")) # CAUTIOUSLY! Not to overwrite!



# 5. Start tuning for success_treshold:
set.seed(1234)
my_success_treshold <- c(0.011, 0.021, 0.031, 0.041, 0.051)
eo_list_out_5 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = my_success_treshold, a = 0.3,
                                                 k_max = 4, tournament_part = 0.5,
                                                 M = 15, max_iter = 1000, max_f_calls = 10000) # PC 3 h 20 min
#save(eo_list_out_5, file=paste0("data/experiment", perform_experiment, "/eo_list_out_5.Rdata")) # CAUTIOUSLY! Not to overwrite!



# 6. Start tuning for init method:
set.seed(1234)
my_init <- c("random", "random_close", "id_close")
eo_list_out_6 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.031, a = 0.3,
                                                 k_max = 4, tournament_part = 0.5, init = my_init,
                                                 M = 15, max_iter = 1000, max_f_calls = 10000) # PC 70 min
#save(eo_list_out_6, file=paste0("data/experiment", perform_experiment, "/eo_list_out_6.Rdata")) # CAUTIOUSLY! Not to overwrite!



# 7. Start tuning for init method (bigger budget):
set.seed(1234)
my_init <- c("random", "random_close", "id_close")
eo_list_out_7 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.031, a = 0.3,
                                                 k_max = 4, tournament_part = 0.5, init = my_init,
                                                 M = 5, max_iter = 1000, max_f_calls = 100000) # PC 3 h 50 min
#save(eo_list_out_7, file=paste0("data/experiment", perform_experiment, "/eo_list_out_7.Rdata")) # CAUTIOUSLY! Not to overwrite!



