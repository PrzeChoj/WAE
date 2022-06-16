library(magrittr)
source("R/algorithm.R") # devtools::install_github("PrzeChoj/gips", ref = "91ce43e068f")

set.seed(1234)
perform_experiment <- "1" # "0" or "1" or "2" or "3" or "4"

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
  # heatmap(sigma_matrix, Rowv = NA, Colv = NA)                             # this is how this matrix looks like

# prod(1:perm_size) / 100 / 60 / 60  # liczba godzin potrzebnych do przejrzenia calej dziedziny

my_goal_function <- goal_function_maker(perm_size, n_number, sigma = sigma_matrix)
U <- attr(my_goal_function, "U")
# heatmap(U, Rowv = NA, Colv = NA)                               # this is esitimated matrix looks like

(f_val_max <- my_goal_function(perm_real))       # 0 --->>> -28.5; 1 --->>> 173.8; 2 --->>>  56.00;  3 --->>> -41.5995; 4 --->>> 24159
(f_val_id <- my_goal_function(permutations::id)) # 0 --->>> -45.8; 1 --->>> 79.5;  2 --->>> -108.26; 3 --->>> -194.46;  4 --->>> 22512


# Reference algorithms (for experiments 1, 2, 3):
load(paste0("data/experiment", perform_experiment, "/mh_list1e4.Rdata"))
load(paste0("data/experiment", perform_experiment, "/mh_list1e5.Rdata"))       # non existing for experiment 3
load(paste0("data/experiment", perform_experiment, "/mc_list.Rdata"))
load(paste0("data/experiment", perform_experiment, "/bg_start_id_list.Rdata"))




#####################################################################################

###################### experiment 0

load(file=paste0("data/experiment", perform_experiment, "/eo_list_test.Rdata"))

jpeg("plots/exp0_1.jpeg", width=810, height=600)
plot_ecdf_list_single(eo_list_test, experiment = perform_experiment,
                      my_title = "Wzrost znalezionego rozwiązania dla wielu wywołań EO w małej przestrzeni z p = 6",
                      my_xlab = "log10 liczby wywołań funkcji celu",
                      my_sub = "dla różnych wywołań algorytmu",
                      my_ylab = "Przeskalowana wartość funkcji celu",
                      text_cex = 1.5)
dev.off()
# implementation is correct, always found the best or near the best permutation in this small space



# Tuning experiments



###################### experiment 1

# 1. a:
my_a <- c(0.1, 0.3, 0.5, 1)
load(paste0("data/experiment", perform_experiment, "/eo_list_out_1.Rdata"))

eo_list_out_1_appended <- append_the_list(eo_list_out_1, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp1_1_1.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_1_appended, paste0("a = ", my_a), experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 1; Dobór parametru a",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()


# the best is a = 0.3
jpeg("plots/exp1_1_2.jpeg", width=810, height=600)
par(mfrow = c(1,2))
plot_ecdf_list_single(mh_list1e4[1:10], my_title = "Eksperyment 1; MH; Wiele wywołań",
                      my_xlab = "log10 liczby wywołań funkcji celu",
                      my_sub = "",
                      my_ylab = "Przeskalowana wartość funkcji celu",
                      text_cex = 1.5,
                      legend_cex = 1.5, lines_transparency=0.5)
plot_ecdf_list_single(eo_list_out_1[[2]],
                      my_title = paste0("Eksperyment 1; EO, a = ",
                                        my_a[2], "; Wiele wywołań"), experiment = perform_experiment,
                      my_xlab = "log10 liczby wywołań funkcji celu",
                      my_sub = "",
                      my_ylab = "Przeskalowana wartość funkcji celu",
                      text_cex = 1.5,
                      legend_cex = 1.5, lines_transparency=0.5)
par(mfrow = c(1,1))
dev.off()



# 2. k_max:
my_k_max <- c(1, 2, 3, 4, 7, 14, 20)
load(paste0("data/experiment", perform_experiment, "/eo_list_out_2.Rdata"))

eo_list_out_2_appended <- append_the_list(eo_list_out_2, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp1_2.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_2_appended, paste0("k_max = ", my_k_max),
               experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 1; Dobór parametru k_max",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()
# the best is k_max = 4, but k = 1 is very close
plot_ecdf_list_single(eo_list_out_2[[4]], # unstable results
                      my_title = paste0("Eksperyment 1; k_max = ",
                                        my_k_max[4]), experiment = perform_experiment)




# 3. pop_size:
my_pop_size <- c(10, 30, 70, 100, 150, 200)
load(paste0("data/experiment", perform_experiment, "/eo_list_out_3.Rdata"))

eo_list_out_3_appended <- append_the_list(eo_list_out_3, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp1_3.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_3_appended, paste0("pop_size = ", my_pop_size),
               experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 1; Dobór parametru pop_size",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()

# NOTE MH is only 1:10. Feel free to extend it to 1:100
par(mfrow = c(1,4))
plot_ecdf_list_single(mh_list1e4[1:10], my_title = "ECDF plot: MH")
# the best is pop_size = 100; as we used to. pop_size = 150 is very close
plot_ecdf_list_single(eo_list_out_3[[1]], # transparently worse results
                      my_title = paste0("ECDF plot: pop_size = ",
                                        my_pop_size[1]))
plot_ecdf_list_single(eo_list_out_3[[4]], # unstable results
                      my_title = paste0("ECDF plot: pop_size = ",
                                        my_pop_size[4]))
plot_ecdf_list_single(eo_list_out_3[[6]], # transparently worse results
                      my_title = paste0("ECDF plot: pop_size = ",
                                        my_pop_size[6]))
par(mfrow = c(1,1))




# 4. tournament_part:
my_tournament_part <- c(0.07, 0.11, 0.2, 0.35, 0.5, 0.65)
load(paste0("data/experiment", perform_experiment, "/eo_list_out_4.Rdata"))

eo_list_out_4_appended <- append_the_list(eo_list_out_4, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp1_4.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_4_appended, paste0("tournament_part = ", my_tournament_part),
               experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 1; Dobór parametru tournament_part",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()

par(mfrow = c(1,3))
# the best is tournament_part = 0.5
plot_ecdf_list_single(eo_list_out_4[[5]], # 0.5; good results
                      my_title = paste0("ECDF plot: tournament_part = ",
                                        my_tournament_part[5]))
plot_ecdf_list_single(eo_list_out_4[[4]], # 0.35; good results
                      my_title = paste0("ECDF plot: tournament_part = ",
                                        my_tournament_part[4]))
plot_ecdf_list_single(eo_list_out_4[[3]], # 0.2 ; bad results
                      my_title = paste0("ECDF plot: tournament_part = ",
                                        my_tournament_part[3]))
par(mfrow = c(1,1))








# 5. success_treshold:
my_success_treshold <- c(0.011, 0.021, 0.031, 0.041, 0.051)
load(paste0("data/experiment", perform_experiment, "/eo_list_out_5.Rdata"))

eo_list_out_5_appended <- append_the_list(eo_list_out_5, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp1_5.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_5_appended, paste0("success_treshold = ", my_success_treshold),
               experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 1; Dobór parametru success_treshold",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()
# the best is success_treshold = 0.031

par(mfrow = c(1,3))
plot_ecdf_list_single(eo_list_out_5[[1]], # 0.11; bad results
                      my_title = paste0("ECDF plot: success_treshold = ",
                                        my_success_treshold[1]))
plot_ecdf_list_single(eo_list_out_5[[3]], # 0.31; good results
                      my_title = paste0("ECDF plot: success_treshold = ",
                                        my_success_treshold[3]))
plot_ecdf_list_single(eo_list_out_5[[4]], # 0.41; good results too
                      my_title = paste0("ECDF plot: success_treshold = ",
                                        my_success_treshold[4]))
par(mfrow = c(1,1))




# 6. init method:
my_init <- c("random", "random_close", "id_close")
load(paste0("data/experiment", perform_experiment, "/eo_list_out_6.Rdata"))

eo_list_out_6_appended <- append_the_list(eo_list_out_6, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

plot_ecdf_list(eo_list_out_6_appended, paste0("initization method = ", my_init), legend_cex = 0.55)

par(mfrow = c(1,3))
plot_ecdf_list_single(eo_list_out_6[[1]], my_title = paste0("ECDF plot: init ", my_init[1]))
plot_ecdf_list_single(eo_list_out_6[[2]], my_title = paste0("ECDF plot: init ", my_init[2]))
plot_ecdf_list_single(eo_list_out_6[[3]], my_title = paste0("ECDF plot: init ", my_init[3]))
par(mfrow = c(1,1))




# 7. init method (bigger budget):
my_init <- c("random", "random_close", "id_close")
load(paste0("data/experiment", perform_experiment, "/eo_list_out_7.Rdata"))

eo_list_out_7_appended <- append_the_list(eo_list_out_7, list(mh_list1e5, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp1_7.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_7_appended, paste0("init = ", my_init),
               experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 1; Dobór sposobu inicjacji",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()

# H_0 = "random_close and MH have the same mean on 1e5"
t.test(1:10 %>% sapply(function(i){max(mh_list1e5[[i]])}),
       1:5 %>% sapply(function(i){max(eo_list_out_7[[2]][[i]])}))  # p_val = 0.12

par(mfrow = c(1,3))
plot_ecdf_list_single(eo_list_out_7[[1]], my_title = paste0("ECDF plot: init ", my_init[1]))
plot_ecdf_list_single(eo_list_out_7[[2]], my_title = paste0("ECDF plot: init ", my_init[2]))
plot_ecdf_list_single(eo_list_out_7[[3]], my_title = paste0("ECDF plot: init ", my_init[3]))
par(mfrow = c(1,1))
# The best is init = "random_close".
  # Although, the differences does not seem big. The init = "random_close" had 1 unlucky run,
  # other runs were very good. The init = "random" had 2 unlucky runs, but not as unlucky as "random_close".
# More tests would be awesome, but the computing time like this is enormous for us.




##############################################################################################################################




###################### experiment 2



# 1. a:
my_a <- c(0.1, 0.3, 0.5, 1)
load(paste0("data/experiment", perform_experiment, "/eo_list_out_1.Rdata"))

eo_list_out_1_appended <- append_the_list(eo_list_out_1, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp2_1.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_1_appended, paste0("a = ", my_a), experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 2; Dobór parametru a",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()
# the best is a = 0.3; the same conclusion as in experiment 1
plot_ecdf_list_single(eo_list_out_1[[2]],
                      my_title = paste0("ECDF plot: a = ", my_a[2]),
                      experiment = perform_experiment)




# 2. k_max:
my_k_max <- c(1, 2, 3, 4, 7, 14, 20)
load(paste0("data/experiment", perform_experiment, "/eo_list_out_2.Rdata"))

eo_list_out_2_appended <- append_the_list(eo_list_out_2, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp2_2.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_2_appended, paste0("k_max = ", my_k_max),
               experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 2; Dobór parametru k_max",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()
# the best is k_max = 7, but k = 20 is very close. Strange, k=20 is very random...

par(mfrow = c(1,3))
plot_ecdf_list_single(eo_list_out_2[[4]], # unstable results
                      my_title = paste0("ECDF plot: k_max = ", my_k_max[4]),
                      experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_2[[5]], # transparently worse results
                      my_title = paste0("ECDF plot: k_max = ", my_k_max[5]),
                      experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_2[[7]], # transparently worse results
                      my_title = paste0("ECDF plot: k_max = ", my_k_max[7]),
                      experiment = perform_experiment)
par(mfrow = c(1,1))




# 3. pop_size:
my_pop_size <- c(10, 30, 70, 100, 150, 200)
load(paste0("data/experiment", perform_experiment, "/eo_list_out_3.Rdata"))

eo_list_out_3_appended <- append_the_list(eo_list_out_3, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp2_3.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_3_appended, paste0("pop_size = ", my_pop_size),
               experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 2; Dobór parametru pop_size",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()

par(mfrow = c(1,3))
# pop_size %in% c(70, 100, 150, 200) are very close to each other
plot_ecdf_list_single(eo_list_out_3[[3]], # unstable results
                      my_title = paste0("ECDF plot: pop_size = ", my_pop_size[3]),
                      experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_3[[4]], # unstable results
                      my_title = paste0("ECDF plot: pop_size = ", my_pop_size[4]),
                      experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_3[[6]], # unstable results
                      my_title = paste0("ECDF plot: pop_size = ", my_pop_size[6]),
                      experiment = perform_experiment)
par(mfrow = c(1,1))




# 4. tournament_part:
my_tournament_part <- c(0.07, 0.11, 0.2, 0.35, 0.5, 0.65)
load(paste0("data/experiment", perform_experiment, "/eo_list_out_4.Rdata"))

eo_list_out_4_appended <- append_the_list(eo_list_out_4, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp2_4.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_4_appended, paste0("tournament_part = ", my_tournament_part),
               experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 2; Dobór parametru tournament_part",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()
# the best is tournament_part = 0.5

par(mfrow = c(1,3))
plot_ecdf_list_single(eo_list_out_4[[3]], # 0.2
                      my_title = paste0("ECDF plot: tournament_part = ", my_tournament_part[3]),
                      experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_4[[4]], # 0.35; najlepsze wyniki i najmniejsze rozproszenie
                      my_title = paste0("ECDF plot: tournament_part = ", my_tournament_part[4]),
                      experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_4[[5]], # 0.5; w miarę ok wyniki, ale duże rozproszenie
                      my_title = paste0("ECDF plot: tournament_part = ", my_tournament_part[5]),
                      experiment = perform_experiment)
par(mfrow = c(1,1))





# 5. success_treshold:
my_success_treshold <- c(0.011, 0.021, 0.031, 0.051, 0.071)
load(paste0("data/experiment", perform_experiment, "/eo_list_out_5.Rdata"))

eo_list_out_5_appended <- append_the_list(eo_list_out_5, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp2_5.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_5_appended, paste0("success_treshold = ", my_success_treshold),
               experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 2; Dobór parametru success_treshold",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()
# the best is success_treshold = 0.031

par(mfrow = c(1,3))
plot_ecdf_list_single(eo_list_out_5[[1]], # 0.11; bad results
                      my_title = paste0("ECDF plot: success_treshold = ", my_success_treshold[1]),
                      experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_5[[3]], # 0.31; good results
                      my_title = paste0("ECDF plot: success_treshold = ", my_success_treshold[3]),
                      experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_5[[5]], # 0.71; good results too; same lines excelent, some less lucky..
                      my_title = paste0("ECDF plot: success_treshold = ", my_success_treshold[5]),
                      experiment = perform_experiment)
par(mfrow = c(1,1))





# 6. init method:
my_init <- c("random", "random_close", "id_close")
load(paste0("data/experiment", perform_experiment, "/eo_list_out_6.Rdata"))

eo_list_out_6_appended <- append_the_list(eo_list_out_6, list(mh_list1e4, mc_list,
                                                              bg_start_id_list))

par(mfrow = c(2,2))
plot_ecdf_list(eo_list_out_6_appended, paste0("initization method = ", my_init), legend_cex = 0.55, experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_6[[1]], my_title = paste0("ECDF plot: init ", my_init[1]), experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_6[[2]], my_title = paste0("ECDF plot: init ", my_init[2]), experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_6[[3]], my_title = paste0("ECDF plot: init ", my_init[3]), experiment = perform_experiment)
par(mfrow = c(1,1))





# 7. init method (bigger budget):
my_init <- c("random", "random_close", "id_close")
load(paste0("data/experiment", perform_experiment, "/eo_list_out_7.Rdata"))

eo_list_out_7_appended <- append_the_list(eo_list_out_7, list(mh_list1e5, mc_list,
                                                              bg_start_id_list))

jpeg("plots/exp2_7_1.jpeg", width=810, height=600)
plot_ecdf_list(eo_list_out_7_appended, paste0("init = ", my_init),
               experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 2; Dobór sposobu inicjacji",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()

jpeg("plots/exp2_7_2.jpeg", width=1210, height=600)
par(mfrow = c(1,3))
plot_ecdf_list_single(eo_list_out_7[[1]], experiment = perform_experiment,
                      my_title = "Eksperyment 2; EO; Inicjacja init_random",
                      my_xlab = "log10 liczby wywołań funkcji celu",
                      my_sub = "",
                      my_ylab = "Przeskalowana wartość funkcji celu",
                      text_cex = 1.75)
plot_ecdf_list_single(eo_list_out_7[[2]], experiment = perform_experiment,
                      my_title = "Eksperyment 2; EO; Inicjacja random_close",
                      my_xlab = "log10 liczby wywołań funkcji celu",
                      my_sub = "",
                      my_ylab = "Przeskalowana wartość funkcji celu",
                      text_cex = 1.75)
plot_ecdf_list_single(eo_list_out_7[[3]], experiment = perform_experiment,
                      my_title = "Eksperyment 2; EO; Inicjacja id_close",
                      my_xlab = "log10 liczby wywołań funkcji celu",
                      my_sub = "",
                      my_ylab = "Przeskalowana wartość funkcji celu",
                      text_cex = 1.75)
par(mfrow = c(1,1))
dev.off()
# Now, init = "random_close" is not the best any more. We see one outsider in it.
  # However, even if we get rid of the outsider, it is still worse than "id_colse":

eo_list_out_7[[2]][[3]] <- NULL
eo_list_out_7_appended <- append_the_list(eo_list_out_7, list(mh_list1e5, mc_list, # TODO(mh_list1e5)
                                                              bg_start_id_list))

par(mfrow = c(2,2))
plot_ecdf_list(eo_list_out_7_appended, paste0("initization method = ", my_init), legend_cex = 0.55, experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_7[[1]], my_title = paste0("ECDF plot: init ", my_init[1]), experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_7[[2]], my_title = paste0("ECDF plot: init ", my_init[2]), experiment = perform_experiment)
plot_ecdf_list_single(eo_list_out_7[[3]], my_title = paste0("ECDF plot: init ", my_init[3]), experiment = perform_experiment)
par(mfrow = c(1,1))
# More tests would be awesome, but the computing time like this is enormous for us.

# H_0 = "random_close and MH have the same mean on 1e5"
t.test(1:10 %>% sapply(function(i){max(mh_list1e5[[i]])}),
       1:5 %>% sapply(function(i){max(eo_list_out_7[[1]][[i]])}))  # init=random --->>> p_val = 0.054
t.test(1:10 %>% sapply(function(i){max(mh_list1e5[[i]])}),
       1:5 %>% sapply(function(i){max(eo_list_out_7[[3]][[i]])}))  # init=id_close --->>> p_val = 0.016




##############################################################################################################################




###################### experiment 3



load(paste0("data/experiment", perform_experiment, "/eo_list_init.Rdata"))

jpeg("plots/exp3_1.jpeg", width=810, height=600)
plot_ecdf_list(list(mh_list1e4, mc_list, bg_start_id_list, eo_list_random_close, eo_list_id_close),
               c("EO random close", "EO id close"),
               experiment = perform_experiment,
               my_title = "Wykres ECDF; Eksperyment 3; Średnia z 200 powtórzeń",
               my_xlab = "log10 liczby wywołań funkcji celu",
               my_sub = "",
               my_ylab = "Przeskalowana wartość funkcji celu",
               text_cex = 1.5,
               legend_cex = 1.5)
dev.off()
# EO działa znacząco lepiej, ale dopiero po 10^3.5 = 3200

# TODO(Ustawiłem 1:10, bo 1:200 bardzo lagowało. Zrobić linie ciut przezroczycte, średnia dokładna, szare przedziały ufności)
xlog <- TRUE
lines_transparency <- 0.05
jpeg("plots/exp3_2.jpeg", width=1210, height=600)
par(mfrow = c(1,3))
plot_ecdf_list_single(mh_list1e4, my_title = "Eksperyment 3; MH; Wiele wywołań",
                      experiment = perform_experiment, xlog = xlog,
                      my_xlab = "log10 liczby wywołań funkcji celu",
                      my_sub = "",
                      my_ylab = "Przeskalowana wartość funkcji celu",
                      text_cex = 1.5,
                      legend_cex = 1.5, lines_transparency=lines_transparency,
                      line_colours="#C3F73A")
plot_ecdf_list_single(eo_list_random_close, my_title = "Eksperyment 3; EO, random close; Wiele wywołań",
                      experiment = perform_experiment, xlog = xlog,
                      my_xlab = "log10 liczby wywołań funkcji celu",
                      my_sub = "",
                      my_ylab = "Przeskalowana wartość funkcji celu",
                      text_cex = 1.5,
                      legend_cex = 1.5, lines_transparency=lines_transparency,
                      line_colours="#5ABCB9")
plot_ecdf_list_single(eo_list_id_close, my_title = "Eksperyment 3; EO, id close; Wiele wywołań",
                      experiment = perform_experiment, xlog = xlog,
                      my_xlab = "log10 liczby wywołań funkcji celu",
                      my_sub = "",
                      my_ylab = "Przeskalowana wartość funkcji celu",
                      text_cex = 1.5,
                      legend_cex = 1.5, lines_transparency=lines_transparency,
                      line_colours="#254441")
par(mfrow = c(1,1))
dev.off()


t.test(1:100 %>% sapply(function(i){max(mh_list1e4[[i]])}),
       1:200 %>% sapply(function(i){max(eo_list_random_close[[i]])})) # p-val < 2.2 * 10 ^ (-16), so definitely EO is better
t.test(1:100 %>% sapply(function(i){max(mh_list1e4[[i]])}),
       1:200 %>% sapply(function(i){max(eo_list_id_close[[i]])})) # p-val < 2.2 * 10 ^ (-16), so definitely EO is better
t.test(1:200 %>% sapply(function(i){max(eo_list_random_close[[i]])}),
       1:200 %>% sapply(function(i){max(eo_list_id_close[[i]])})) # p-val = 0.5607, so no conclusions can be made on the betterness




##############################################################################################################################




###################### experiment 4


load(paste0("data/experiment", perform_experiment, "/mh_full.Rdata"))
load(paste0("data/experiment", perform_experiment, "/eo_full_1.Rdata"))
load(paste0("data/experiment", perform_experiment, "/eo_full_2.Rdata"))

jpeg("plots/exp4_1.jpeg", width=1080, height=800)
plot_ecdf(list(list(mh$goal_function_logvalues), # Mac 10 seconds
               list(eo1$goal_function_logvalues),
               list(eo2$goal_function_logvalues)),
          my_title = "Wykres ECDF dla przestrzeni z p = 100",
          my_xlab = "log10 liczby wywołań funkcji celu",
          my_sub = "dla różnych algorytmów",
          my_ylab = "Przeskalowana wartość funkcji celu",
          text_cex = 1.5,
          legend_text = c("MH", "EO id close", "EO random close"),
          legend_cex = 1.2, ECDF = FALSE,
          min_val = 3504, max_val = 24159, reference_line = 22512)
dev.off()




