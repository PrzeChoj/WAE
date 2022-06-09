library(magrittr)
source("R/algorithm.R")

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



# Tuning experiments

# 1. Start with small tuning for a:
set.seed(1234)
my_a <- c(0.1, 0.3, 0.5, 1)
eo_list_out_1 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.025, a = my_a,
                                                 k_max = 5, tournament_size = 50,
                                                 M = 10, max_iter = 100) # PC 70 min
#save(eo_list_out_1, file="data/eo_list_out_1.Rdata") # UWAGA! nie nadpisac!
load("data/eo_list_out_1.Rdata")


eo_list_out_1[[length(eo_list_out_1) + 1]] <- mh_list

plot_ecdf(eo_list_out_1, min_val = f_val_id, max_val = f_val_max,
          legend_text = c(paste0("a = ", my_a), "MH"))
# the best is a = 0.3



# 2. Start tuning for k_max:
set.seed(1234)
my_k_max <- c(1, 2, 3, 4, 7, 14, 20)
eo_list_out_2 <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.025, a = 0.3,
                                                 k_max = my_k_max, tournament_size = 50,
                                                 M = 30, max_iter = 100) # PC 6 hours 20 minutes
#save(eo_list_out_2, file="data/eo_list_out_2.Rdata") # UWAGA! nie nadpisac!
load("data/eo_list_out_2.Rdata")


eo_list_out_2[[length(eo_list_out_2) + 1]] <- mh_list

plot_ecdf(eo_list_out_2, min_val = f_val_id, max_val = f_val_max,
          legend_text = c(paste0("k_max = ", my_k_max), "MH"))
# the best is k_max = 4



# 3. pop_size?









