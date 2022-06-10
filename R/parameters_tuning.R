library(magrittr)
source("R/algorithm.R") # devtools::install_github("PrzeChoj/gips")

set.seed(1234)

n_number <- 20
perm_size <- 25
# prod(1:perm_size) / 100 / 60 / 60  # liczba godzin potrzebnych do przejrzenia calej dziedziny

my_goal_function <- goal_function_maker(perm_size, n_number)
U <- attr(my_goal_function, "U")

perm_real <- as.cycle(1:perm_size)

(f_val_max <- my_goal_function(perm_real)) # 173.8
(f_val_id <- my_goal_function(permutations::id)) # 79.5



# MH for reference
mh_list <- get_list_of_log_values_MH(my_goal_function, max_iter = 10000, M = 100) # PC 120 min
#save(mh_list, file="data/mh_list.Rdata") # UWAGA! nie nadpisac!
load("data/mh_list.Rdata")



# MC for reference
set.seed(1234)
mc_list <- get_list_of_log_values_MC(my_goal_function, max_iter = 10000, M = 100) # PC 70 min
#save(mc_list, file="data/mc_list.Rdata") # UWAGA! nie nadpisac!
load("data/mc_list.Rdata")

f_val_min <- 1:length(mc_list) %>% sapply(function(i){min(mc_list[[i]])}) %>% min # smallest of drawn values
f_val_med <- median(mc_list[[1]]) # median of first part of drawn values



# BG for reference
set.seed(1234)
bg_start_id_list <- list(best_growth(attr(my_goal_function, "U"), n_number = attr(my_goal_function, "n"),
                                     max_iter = 20)[["goal_function_logvalues"]]) # max_iter == 14 is enought; bg_start_id is NOT random; PC 1 min
#save(bg_start_id_list, file="data/bg_start_id_list.Rdata") # UWAGA! nie nadpisac!
load("data/bg_start_id_list.Rdata")

set.seed(1234)
# Use this with caution!!! It is incredibly hard to interpret! Only part of the length of the line is sensible!
bg_start_random_list <- get_list_of_log_values_BG(my_goal_function, max_iter = 100, M = 100) # PC 27 min
#save(bg_start_random_list, file="data/bg_start_random_list.Rdata") # UWAGA! nie nadpisac!
#load("data/bg_start_random_list.Rdata")
#bg_start_random_list_mean <- make_BG_mean(bg_start_random_list) # Use this with caution!!! It is incredibly hard to interpret! Only part of the length of the line is sensible!





# Tuning experiments

# 1. Start with small tuning for a:
set.seed(1234)
my_a <- c(0.1, 0.3, 0.5, 1)
eo_list_out_1 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.025, a = my_a,
                                                 k_max = 5, tournament_part = 0.5,
                                                 M = 10, max_iter = 100) # PC 70 min
#save(eo_list_out_1, file="data/eo_list_out_1.Rdata") # UWAGA! nie nadpisac!
load("data/eo_list_out_1.Rdata")


eo_list_out_1_appended <- append_the_list(eo_list_out_1, list(mh_list, mc_list,
                                                              bg_start_id_list))

plot_ecdf_list(eo_list_out_1_appended, paste0("a = ", my_a))
# the best is a = 0.3
plot_ecdf_list_single(eo_list_out_1[[2]])




# 2. Start tuning for k_max:
set.seed(1234)
my_k_max <- c(1, 2, 3, 4, 7, 14, 20)
eo_list_out_2 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.025, a = 0.3,
                                                 k_max = my_k_max, tournament_part = 0.5,
                                                 M = 30, max_iter = 100) # PC 6 hours 20 minutes
#save(eo_list_out_2, file="data/eo_list_out_2.Rdata") # UWAGA! nie nadpisac!
load("data/eo_list_out_2.Rdata")


eo_list_out_2_appended <- append_the_list(eo_list_out_2, list(mh_list, mc_list,
                                                              bg_start_id_list))

plot_ecdf_list(eo_list_out_2_appended, paste0("k_max = ", my_k_max), legend_cex = 0.8)
# the best is k_max = 4, but k = 1 is very close
plot_ecdf_list_single(eo_list_out_2[[4]]) # unstable results




# 3. Start tuning for pop_size:
set.seed(1234)
my_pop_size <- c(10, 30, 70, 100, 150, 200)
eo_list_out_3 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = my_pop_size,
                                                 success_treshold = 0.025, a = 0.3,
                                                 k_max = 4, tournament_part = 0.5,
                                                 M = 10, max_iter = 1000, max_f_calls = 10000) # PC 2 h
#save(eo_list_out_3, file="data/eo_list_out_3.Rdata") # UWAGA! nie nadpisac!
load("data/eo_list_out_3.Rdata")


eo_list_out_3_appended <- append_the_list(eo_list_out_3, list(mh_list, mc_list,
                                                              bg_start_id_list))


par(mfrow = c(2,2))
plot_ecdf_list(eo_list_out_3_appended, paste0("pop_size = ", my_pop_size), legend_cex = 0.4)
# the best is pop_size = 100; as we used to. pop_size = 150 is very close
plot_ecdf_list_single(eo_list_out_3[[4]]) # unstable results
plot_ecdf_list_single(eo_list_out_3[[1]]) # transparently worse results
plot_ecdf_list_single(eo_list_out_3[[6]]) # transparently worse results
par(mfrow = c(1,1))




# 4. Start tuning for tournament_part:
set.seed(1234)
my_tournament_part <- c(0.07, 0.11, 0.2, 0.35, 0.5, 0.65)
eo_list_out_4 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.025, a = 0.3,
                                                 k_max = 4, tournament_part = my_tournament_part,
                                                 M = 15, max_iter = 1000, max_f_calls = 10000) # PC 3 h?
#save(eo_list_out_4, file="data/eo_list_out_4.Rdata") # UWAGA! nie nadpisac!
load("data/eo_list_out_4.Rdata")


eo_list_out_4_appended <- append_the_list(eo_list_out_4, list(mh_list, mc_list,
                                                              bg_start_id_list))

plot_ecdf_list(eo_list_out_4_appended, paste0("tournament_part = ", my_tournament_part), legend_cex = 0.9)
# the best is tournament_part = 0.5
plot_ecdf_list_single(eo_list_out_4[[4]]) # 0.35; good results
plot_ecdf_list_single(eo_list_out_4[[3]]) # 0.2 ; bad results




# 5. Start tuning for success_treshold:
set.seed(1234)
my_success_treshold <- c(0.011, 0.021, 0.031, 0.041, 0.051)
eo_list_out_5 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = my_success_treshold, a = 0.3,
                                                 k_max = 4, tournament_part = 0.5,
                                                 M = 15, max_iter = 1000, max_f_calls = 10000) # PC 3 h 20 min?
#save(eo_list_out_5, file="data/eo_list_out_5.Rdata") # UWAGA! nie nadpisac!
load("data/eo_list_out_5.Rdata")


eo_list_out_5_appended <- append_the_list(eo_list_out_5, list(mh_list, mc_list,
                                                              bg_start_id_list))

plot_ecdf_list(eo_list_out_5_appended, paste0("success_treshold = ", my_success_treshold), legend_cex = 0.9)
# the best is success_treshold = 0.031
plot_ecdf_list_single(eo_list_out_5[[3]]) # 0.31; good results
plot_ecdf_list_single(eo_list_out_5[[4]]) # 0.41; good results too




# 6. Start tuning for init method:
set.seed(1234)
my_init <- c("random", "random_close", "id_close")
eo_list_out_6 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.031, a = 0.3,
                                                 k_max = 4, tournament_part = 0.5, init = my_init,
                                                 M = 15, max_iter = 1000, max_f_calls = 10000) # PC 70 min
#save(eo_list_out_6, file="data/eo_list_out_6.Rdata") # UWAGA! nie nadpisac!
load("data/eo_list_out_6.Rdata")


eo_list_out_6_appended <- append_the_list(eo_list_out_6, list(mh_list, mc_list,
                                                              bg_start_id_list))

par(mfrow = c(2,2))
plot_ecdf_list(eo_list_out_6_appended, paste0("initization method = ", my_init), legend_cex = 0.55)
# The lines are very close for this budget. Try with bigger budget:
plot_ecdf_list_single(eo_list_out_6[[1]])
plot_ecdf_list_single(eo_list_out_6[[2]])
plot_ecdf_list_single(eo_list_out_6[[3]])
par(mfrow = c(1,1))




# 7. Start tuning for init method (bigger budget):
set.seed(1234)
my_init <- c("random", "random_close", "id_close")
eo_list_out_7 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.031, a = 0.3,
                                                 k_max = 4, tournament_part = 0.5, init = my_init,
                                                 M = 5, max_iter = 1000, max_f_calls = 100000) # PC 4? h
#save(eo_list_out_7, file="data/eo_list_out_7.Rdata") # UWAGA! nie nadpisac!
load("data/eo_list_out_7.Rdata")


eo_list_out_7_appended <- append_the_list(eo_list_out_7, list(mh_list, mc_list,
                                                              bg_start_id_list))

plot_ecdf_list(eo_list_out_7_appended, paste0("initization method = ", my_init))
# the best is init = ?




