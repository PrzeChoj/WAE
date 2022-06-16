library(magrittr)
source("R/algorithm.R") # devtools::install_github("PrzeChoj/gips", ref = "91ce43e068f")

set.seed(1234)

# summary time for experiment 0 --->>>  6 minutes; 1 --->>> 24 h; 2 --->>> 17 h; 3 --->>> 10 h; 4 --->>> 10? h
  # all experiments --->>> 61? h
perform_experiment <- "4" # "0", "1" or "2" or "3" or "4"

n_number <- 20
if(perform_experiment == "0"){
  perm_size <- 6
  sigma_matrix <- NULL
  
  perm_real <- as.cycle(1:perm_size)
}else if(perform_experiment == "1"){
  perm_size <- 25
  sigma_matrix <- NULL
  
  perm_real <- as.cycle(1:perm_size)
}else if(perform_experiment == "2"){
  perm_size <- 26
  
  # generate sigma matrix:
  org_sigma_matrix <- sigma_maker(13)
  sigma_matrix <- matrix(numeric(perm_size*perm_size), nrow = perm_size)
  
  for(i in 1:13){
    for(j in 1:13){
      sigma_matrix[i,    j   ] <-   org_sigma_matrix[i,j]
      sigma_matrix[i+13, j+13] <-   org_sigma_matrix[i,j]
      sigma_matrix[i,    j+13] <- - org_sigma_matrix[i,j] / 2
      sigma_matrix[i+13, j   ] <- - org_sigma_matrix[i,j] / 2
    }
  }
  
  perm_real <- as.cycle(as.word(c(2:13, 1, 15:26, 14)))
}else if(perform_experiment == "3"){
  perm_size <- 21
  
  # generate sigma matrix:
  org_sigma_matrix <- sigma_maker(7)
  sigma_matrix <- matrix(numeric(perm_size*perm_size), nrow = perm_size)
  
  for(i in 1:7){
    for(j in 1:7){
      sigma_matrix[i,    j   ] <-   org_sigma_matrix[i,j]
      sigma_matrix[i+7,  j+7 ] <-   org_sigma_matrix[i,j]
      sigma_matrix[i+14, j+14] <-   org_sigma_matrix[i,j]
      sigma_matrix[i,    j+7 ] <- - org_sigma_matrix[i,j] / 2
      sigma_matrix[i,    j+14] <- - org_sigma_matrix[i,j] / 4
      sigma_matrix[i+7,  j   ] <- - org_sigma_matrix[i,j] / 2
      sigma_matrix[i+7,  j+14] <- - org_sigma_matrix[i,j] / 2
      sigma_matrix[i+14, j   ] <- - org_sigma_matrix[i,j] / 4
      sigma_matrix[i+14, j+7 ] <- - org_sigma_matrix[i,j] / 2
    }
  }
  
  perm_real <- as.cycle(as.word(c(2:7, 1, 9:14, 8, 16:21, 15)))
}else if(perform_experiment == "4"){
  n_number <- 200
  perm_size <- 100
  sigma_matrix <- NULL
  
  perm_real <- as.cycle(1:perm_size)
}else{
  stop("Wrong experiment selected!")
}

# check properties:
  # eigen(sigma_matrix)$values                                              # all positive; matrix is positive-defined; matrix can be used as CoV matrix
  # gips::project_matrix(sigma_matrix, perm_real, perm_size) - sigma_matrix # matrix of zeros; this matrix is invariant under perm_real
  # heatmap(sigma_matrix, Rowv=NA, Colv = NA)                               # this is how this matrix looks like

# prod(1:perm_size) / 100 / 60 / 60  # liczba godzin potrzebnych do przejrzenia calej dziedziny

my_goal_function <- goal_function_maker(perm_size, n_number, sigma = sigma_matrix)
U <- attr(my_goal_function, "U")
# heatmap(U, Rowv=NA, Colv = NA)                               # this is how this U matrix looks like

(f_val_max <- my_goal_function(perm_real))       # 0 --->>> -28.5; 1 --->>> 173.8; 2 --->>>  56.00;  3 --->>> -41.5995; 4 --->>> 24159
(f_val_id <- my_goal_function(permutations::id)) # 0 --->>> -45.8; 1 --->>> 79.5;  2 --->>> -108.26; 3 --->>> -194.46;  4 --->>> 22512




################### For experiments 1, 2 and 3:

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

(f_val_min <- 1:length(mc_list) %>% sapply(function(i){min(mc_list[[i]])}) %>% min) # smallest of drawn values
(f_val_med <- median(mc_list[[1]])) # median of first part of drawn values


# BG for reference
set.seed(1234)
bg_start_id_list <- list(best_growth(attr(my_goal_function, "U"), n_number = attr(my_goal_function, "n"),
                                     max_iter = 100)[["goal_function_logvalues"]]) # max_iter == 14 is enough; bg_start_id is NOT random; PC 1 min
#save(bg_start_id_list, file=paste0("data/experiment", perform_experiment, "/bg_start_id_list.Rdata")) # CAUTIOUSLY! Not to overwrite!

set.seed(1234)
# Use this with caution!!! It is incredibly hard to interpret! Only part of the length of the line is sensible!
#bg_start_random_list <- get_list_of_log_values_BG(my_goal_function, max_iter = 100, M = 100) # PC 27 min
#save(bg_start_random_list, file=paste0("data/experiment", perform_experiment, "/bg_start_random_list.Rdata")) # CAUTIOUSLY! Not to overwrite!
#bg_start_random_list_mean <- make_BG_mean(bg_start_random_list) # Use this with caution!!! It is incredibly hard to interpret! Only part of the length of the line is sensible!




###########################################################################################################################




###################### experiment 0
# check the correctness of the implementation

set.seed(1234)
my_init <- c("random", "random")
eo_list_out_test <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                    success_treshold = 0.025, a = 1,
                                                    k_max = 5, tournament_part = 0.5, init = my_init,
                                                    M = 20, max_iter = 1000, max_f_calls = 1000) # PC 6 minuts
eo_list_test <- list() # combine lists
for(my_list in eo_list_out_test[[1]]){
  eo_list_test[[length(eo_list_test) + 1]] <- my_list
}
for(my_list in eo_list_out_test[[2]]){
  eo_list_test[[length(eo_list_test) + 1]] <- my_list
}
#save(eo_list_test, file=paste0("data/experiment", perform_experiment, "/eo_list_test.Rdata")) # CAUTIOUSLY! Not to overwrite!



# Tuning parameters




###################### experiment 1

# 1. Start tuning for a:
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










########################################################################################################################


###################### experiment 2


# 1. Start tuning for a:
set.seed(1234)
my_a <- c(0.1, 0.3, 0.5, 1)
eo_list_out_1 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.031, a = my_a,
                                                 k_max = 4, tournament_part = 0.5,
                                                 M = 10, max_iter = 100, init = "random_close") # PC 60 min
#save(eo_list_out_1, file=paste0("data/experiment", perform_experiment, "/eo_list_out_1.Rdata")) # CAUTIOUSLY! Not to overwrite!




# 2. Start tuning for k_max:
set.seed(1234)
my_k_max <- c(1, 2, 3, 4, 7, 14, 20)
eo_list_out_2 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.031, a = 0.3,
                                                 k_max = my_k_max, tournament_part = 0.5,
                                                 M = 10, max_iter = 100, init = "random_close") # PC 1 hour 55 minutes
#save(eo_list_out_2, file=paste0("data/experiment", perform_experiment, "/eo_list_out_2.Rdata")) # CAUTIOUSLY! Not to overwrite!




# 3. Start tuning for pop_size:
set.seed(1234)
my_pop_size <- c(10, 30, 70, 100, 150, 200)
eo_list_out_3 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = my_pop_size,
                                                 success_treshold = 0.031, a = 0.3,
                                                 k_max = 7, tournament_part = 0.5,
                                                 M = 10, max_iter = 1000, max_f_calls = 10000,
                                                 init = "random_close") # PC 2 h 15 min
#save(eo_list_out_3, file=paste0("data/experiment", perform_experiment, "/eo_list_out_3.Rdata")) # CAUTIOUSLY! Not to overwrite!




# 4. Start tuning for tournament_part:
set.seed(1234)
my_tournament_part <- c(0.07, 0.11, 0.2, 0.35, 0.5, 0.65)
eo_list_out_4 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.031, a = 0.3,
                                                 k_max = 7, tournament_part = my_tournament_part,
                                                 M = 10, max_iter = 1000, max_f_calls = 10000) # PC 2 h 30 min
#save(eo_list_out_4, file=paste0("data/experiment", perform_experiment, "/eo_list_out_4.Rdata")) # CAUTIOUSLY! Not to overwrite!




# 5. Start tuning for success_treshold:
set.seed(1234)
my_success_treshold <- c(0.011, 0.021, 0.031, 0.051, 0.071) # Different from experiment 1 (0.041 --->>> 0.071), because those in exp 1 were very close togather
eo_list_out_5 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = my_success_treshold, a = 0.3,
                                                 k_max = 7, tournament_part = 0.35,
                                                 M = 10, max_iter = 1000, max_f_calls = 10000,
                                                 init = "random_close") # PC 1 h 45 min
#save(eo_list_out_5, file=paste0("data/experiment", perform_experiment, "/eo_list_out_5.Rdata")) # CAUTIOUSLY! Not to overwrite!




# 6. Start tuning for init method:
set.seed(1234)
my_init <- c("random", "random_close", "id_close")
eo_list_out_6 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.031, a = 0.3,
                                                 k_max = 7, tournament_part = 0.35, init = my_init,
                                                 M = 10, max_iter = 1000, max_f_calls = 10000) # PC 60 min
#save(eo_list_out_6, file=paste0("data/experiment", perform_experiment, "/eo_list_out_6.Rdata")) # CAUTIOUSLY! Not to overwrite!




# 7. Start tuning for init method (bigger budget):
set.seed(1234)
my_init <- c("random", "random_close", "id_close")
eo_list_out_7 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.031, a = 0.3,
                                                 k_max = 7, tournament_part = 0.35, init = my_init,
                                                 M = 5, max_iter = 1000, max_f_calls = 100000) # PC 3 h 25 min
#save(eo_list_out_7, file=paste0("data/experiment", perform_experiment, "/eo_list_out_7.Rdata")) # CAUTIOUSLY! Not to overwrite!








########################################################################################################################


###################### experiment 3

# compare inits: "random_close" and "id_close" as a long sample:
set.seed(1234)
my_init <- c("random_close", "id_close")
eo_list_out_long <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                    success_treshold = 0.031, a = 0.3,
                                                    k_max = 7, tournament_part = 0.35, init = my_init,
                                                    M = 200, max_iter = 1000, max_f_calls = 10000) # PC 10 h

eo_list_random_close <- eo_list_out_long[[1]]
eo_list_id_close <- eo_list_out_long[[2]]
#save(eo_list_random_close, eo_list_id_close, file=paste0("data/experiment", perform_experiment, "/eo_list_init.Rdata")) # CAUTIOUSLY! Not to overwrite!


########################################################################################################################


###################### experiment 4

# MH for reference
set.seed(1234)
time_start <- Sys.time()
mh <- gips::MH(U = U, n_number = n, max_iter = 100000) # PC 100 000 --->>> 4 h
time_end <- Sys.time()
print(time_end - time_start)
mh_values <- mh[["goal_function_logvalues"]]
#save(mh_values, file=paste0("data/experiment", perform_experiment, "/mh.Rdata")) # CAUTIOUSLY! Not to overwrite!
#save(mh, file=paste0("data/experiment", perform_experiment, "/mh_full.Rdata")) # CAUTIOUSLY! Not to overwrite!


# EO
set.seed(1234)
time_start <- Sys.time()
eo1 <- evolutional_optimization(my_goal_function = my_goal_function, pop_size = 100,
                                success_treshold = 0.031, a = 0.3,
                                k_max = 7, tournament_part = 0.5, init = "id_close",
                                max_iter = 1000, max_f_calls = 100000) # PC 2h 10 min
time_end <- Sys.time()
print(time_end - time_start)
#save(eo1, file=paste0("data/experiment", perform_experiment, "/eo_full_1.Rdata")) # CAUTIOUSLY! Not to overwrite!

# slabo działa. Wygląda na minimalizacje, a nie maksymalizacje

set.seed(1234)
time_start <- Sys.time()
eo2 <- evolutional_optimization(my_goal_function = my_goal_function, pop_size = 100,
                                success_treshold = 0.031, a = 0.3,
                                k_max = 7, tournament_part = 0.5, init = "random_close",
                                max_iter = 1000, max_f_calls = 100000) # PC 1h 30 min
time_end <- Sys.time()
print(time_end - time_start)
#save(eo2, file=paste0("data/experiment", perform_experiment, "/eo_full_2.Rdata")) # CAUTIOUSLY! Not to overwrite!





