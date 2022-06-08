library(magrittr)
source("R/algorithm.R")

set.seed(1234)

n_number <- 20
perm_size <- 15
# prod(1:perm_size) / 100 / 60 / 60  # liczba godzin potrzebnych do przejrzenia calej dziedziny

my_goal_function <- goal_function_maker(perm_size, n_number)
U <- attr(my_goal_function, "U")

perm_real <- as.cycle(1:perm_size)

(f_val_max <- my_goal_function(perm_real)) # 42.99
(f_val_id <- my_goal_function(permutations::id)) # -18.88



# Tuning experiments

# Start with small tuning for a:
my_a <- c(0.1, 0.5, 1)
my_list_out_a <- get_list_of_lists_of_log_values(goal_function = my_goal_function, pop_size = 100,
                                                 success_treshold = 0.025, a = my_a,
                                                 k_max = 5, tournament_size = 50,
                                                 M = 5, max_iter = 200)

plot_ecdf(my_list_out_a, min_val = f_val_id, max_val = f_val_max,
          legend_text = paste0("a = ", my_a))
