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
(f_val_100 <- 1:100 %>% # -42.60, -9.06
  sapply(function(x){goal_function(as.cycle(rperm(1, perm_size)),
                                   n_number, U)}) %>% {c("min" = min(.),
                                                         "max" = max(.))})
f_val_min <- f_val_100[1]



# algorytmy od ktorych chcemy znaleść lepszy:
set.seed(1234) # best_growth nie jest losowy, ale set.seed nie zaszkodzi
bg <- gips::best_growth(U, n_number, max_iter = 20) # 12 wystarcza
print(bg, log_value = TRUE)
plot(bg, ylim=c(f_val_id, f_val_max))

set.seed(1234)
M <- 3
# MH on the same number of iterations * M:
mh <- MH(U, n_number, bg$iterations_performed * choose(perm_size, 2) * M)

num_of_algs <- 7
plot(mh, ylim=c(f_val_id, f_val_max), type="best", show_legend=FALSE,
     color = rainbow(num_of_algs)[1])

abline(a=f_val_max, b=0, col=rainbow(num_of_algs)[2])

lines((1:bg[["iterations_performed"]]) * choose(perm_size, 2),
      bg$goal_function_logvalues, col=rainbow(num_of_algs)[3],
      pch=4) # this ended after this many iterations

# Monte Carlo
set.seed(1234)
mc <- monte_carlo(my_goal_function,
                  max_iter = bg$iterations_performed * choose(perm_size, 2) * M)

lines(c(as.numeric(names(mc[["best_f_value_list"]]))),
      mc[["best_f_value_list"]], col=rainbow(num_of_algs)[4],
      lwd=3)


# algorytm ewolucyjny
set.seed(1234)

pop_size <- 100

eo <- evolutional_optimization(
  my_goal_function=my_goal_function,
  max_iter=floor(bg$iterations_performed * choose(perm_size, 2) * M / pop_size),
  pop_size=pop_size,
  tournament_part = 0.5,
  k_max = 1 # raczej ustawiac k_max < perm_size
)
lines(c(as.numeric(names(eo[["best_f_value_list"]])) * pop_size),
      eo[["best_f_value_list"]], col=rainbow(num_of_algs)[5],
      lwd=3)


eo[["best_f_value_list"]]
eo[["success_rate_list"]]
eo[["p_t_list"]]

eo2 <- evolutional_optimization(
  my_goal_function=my_goal_function,
  max_iter=floor(bg$iterations_performed * choose(perm_size, 2) * M / pop_size),
  tournament_part = 0.5,
  pop_size=pop_size, k_max = 2
)
lines(c(as.numeric(names(eo2[["best_f_value_list"]])) * pop_size),
      eo2[["best_f_value_list"]], col=rainbow(num_of_algs)[6],
      lwd=3)


eo2$p_t_list # to jest bardzo duze wciaz


eo3 <- evolutional_optimization(
  my_goal_function=my_goal_function,
  max_iter=floor(bg$iterations_performed * choose(perm_size, 2) * M / pop_size),
  tournament_part = 0.1, a=0.1, success_treshold = 0.08,
  pop_size=pop_size, k_max = 4
)

lines(c(as.numeric(names(eo3[["best_f_value_list"]])) * pop_size),
      eo3[["best_f_value_list"]], col=rainbow(num_of_algs)[7],
      lwd=3)





# TODO list:
  # 1. 
  # 2. EPDF podobne co w symulowanym wyrzazaniu (plotowanie i generowanie)
  # 3. Porównanie k_max = 1, k_max = log(p), k_max = p/2, k_max = p-1
  # 4. ?







