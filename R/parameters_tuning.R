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

plot_ecdf(eo_list_out_1_appended, min_val = f_val_med, max_val = f_val_max, reference_line = f_val_id,
          legend_text = c(paste0("a = ", my_a), "MH", "MC", "BG_id"))
# the best is a = 0.3



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

plot_ecdf(eo_list_out_2_appended, min_val = f_val_med, max_val = f_val_max, reference_line = f_val_id,
          legend_text = c(paste0("k_max = ", my_k_max), "MH", "MC", "BG_id"), legend_cex = 0.8)
# the best is k_max = 4



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

plot_ecdf(eo_list_out_3_appended, min_val = f_val_med, max_val = f_val_max, reference_line = f_val_id,
          legend_text = c(paste0("pop_size = ", my_pop_size), "MH", "MC", "BG_id"), legend_cex = 0.9)
# the best is pop_size = ?









