#install.packages("permutations")
library(permutations)

# trzeba trzymac wielkosc tablicy
perm <- as.cycle(as.word(c(1,4,5,2,3,6))) # (1)(3,5)(2,4)(6), czyli 1 i 6 są stałe
permutations::fixed(perm)  # pamięta o 1. Nie pamięta o 6
length(permutations::fixed(perm))  # 5, bo zapomniał o ostatnim



#install.packages("devtools")

#devtools::install_github("PrzeChoj/gips", ref = "91ce43e068f")
library(gips)



# przykladowe zadanie:
source("utilities.R")

set.seed(1234)

p <- 10
n <- 20

example_goal_function <- goal_function_maker(p, n)

actual_permutation <- as.cycle(as.word(c(2:p, 1)))

example_goal_function(permutations::id)   # to jest malo
example_goal_function(actual_permutation) # tego szukamy. To jest max funkcji celu

mh <- MH(U=attr(example_goal_function, "U"), n, max_iter=1000,
         permutations::id, p)  # gips szuka maximum
plot(mh)
max_points <- which(mh$goal_function_logvalues == max(mh$goal_function_logvalues))

found_permutation <- mh$points[[max_points[1]]]  # znalezione maximum
example_goal_function(found_permutation) # To znajduje gips. My chcemy lepiej w tykiej samej liczbie wywolan












