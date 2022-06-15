library(permutations)
library(gips)  # devtools::install_github("PrzeChoj/gips", ref = "91ce43e068f")

sigma_maker <- function(p){
  sigma <- matrix(numeric(p*p), nrow=p)
  for(i in 1:p){
    for(j in 1:p){
      sigma[i,j] <- 1 - min(abs(i-j), p-abs(i-j)) / p
    }
    sigma[i,i] <- 1 + 1/p
  }
  
  sigma
}

U_maker <- function(p, n, sigma=NULL){
  mu <- numeric(p)
  if(is.null(sigma)){
    sigma <- sigma_maker(p)
  }
  
  Z <- MASS::mvrnorm(n, mu = mu, Sigma = sigma)
  U <- t(Z) %*% Z
  
  U
}

goal_function_maker <- function(p, n, sigma=NULL){
  U <- U_maker(p, n, sigma)
  
  my_goal_function <- function(perm){
    goal_function(perm, n, U)
  }
  
  attr(my_goal_function, "U") <- U
  attr(my_goal_function, "n") <- n
  if(is.null(sigma)){
    attr(my_goal_function, "sigma") <- sigma_maker(p)
  }else{
    attr(my_goal_function, "sigma") <- sigma
  }
  
  my_goal_function
}


#' Draw a random permutation
runif_perm <- function(perm_size){
  permutations::as.cycle(permutations::as.word(sample(perm_size, perm_size,
                                                      replace=FALSE)))
}

#' Drawing a random transosition
runif_transposition <- function(perm_size){
  permutations::as.cycle(sample(perm_size, 2, replace=FALSE))
}






#' Trim values to 0-1 range
#' 
trim_values <- function(values, min_val=NULL, max_val=NULL){
  if(is.null(min_val))
    min_val = min(values)
  if(is.null(max_val))
    max_val = max(values)
  
  pmin(1, pmax(0, (cummax(values) - min_val) / (max_val - min_val)))
}

#' Plot ECDF for multiple sets of values
#' 
#' @param values_list list of lists of values of goal_function that were found in the iteration; for a optimization method has to be the same length
#' @param min_val value that will be considered 0
#' @param max_val value that will be considered 1
plot_ecdf <- function(values_list, min_val, max_val, xlog = TRUE,
                      line_colours = "rainbow", max_y_scale = 1,
                      show_legend = TRUE, legend_text = NULL, legend_cex=1,
                      reference_line = NULL, my_title = "ECDF plot",
                      my_xlab = NULL, my_ylab = NULL, my_sub = NULL){
  stopifnot(max_y_scale > 0, max_y_scale <= 1)
  
  num_of_algorithms <- length(values_list)
  num_of_tries <- sapply(values_list, length)
  num_of_iters <- sapply(1:num_of_algorithms, function(i){
    length(values_list[[i]][[1]])
  })
  
  if((length(line_colours) == 1) && (line_colours == "rainbow")){
    line_colours <- rainbow(num_of_algorithms)
  }
  stopifnot(length(line_colours) == num_of_algorithms)
  
  xlim <- c(1, max(num_of_iters))
  if(xlog){
    xlim <- c(0, log10(xlim[2]))
  }
  ylim <- c(0, max_y_scale)
  
  graphics::plot.new()
  graphics::plot.window(xlim, ylim)
  
  if(!is.null(reference_line)){ # the line for value of id permutation
    abline(a = trim_values(reference_line, min_val, max_val), b = 0)
  }
  
  for(i in 1:num_of_algorithms){
    avrage_for_ith_algorithm <- numeric(num_of_iters[i])
    for(j in 1:num_of_tries[i]){
      trimed_values <- trim_values(values_list[[i]][[j]], min_val, max_val)
      avrage_for_ith_algorithm <- avrage_for_ith_algorithm + cummax(trimed_values)
    }
    avrage_for_ith_algorithm <- avrage_for_ith_algorithm / num_of_tries[i]
    
    x_cords <- 1:length(avrage_for_ith_algorithm)
    if(xlog){
      x_cords <- log10(x_cords)
    }
    
    # making this line plot more CDF-like (no slopes):
    x_cords <- c(x_cords[1], rep(x_cords[2:length(x_cords)], each=2))
    y_cords <- c(rep(avrage_for_ith_algorithm[1:(length(avrage_for_ith_algorithm)-1)], each=2),
                 avrage_for_ith_algorithm[length(avrage_for_ith_algorithm)])
    
    graphics::lines.default(x_cords, y_cords,
                            type = "l", col = line_colours[i],
                            lwd=4)
  }
  
  if(xlog){
    xlab <- "log10 of number of function calls"
  }else{
    xlab <- "number of function calls"
  }
  if(!is.null(my_xlab)){
    xlab <- my_xlab
  }
  if(!is.null(my_ylab)){
    ylab <- my_ylab
  }else{
    ylab <- "optimization goals reached"
  }
  if(!is.null(my_sub)){
    sub <- my_sub
  }else{
    sub <- "for different algorithms"
  }
  graphics::title(main = my_title, sub = sub,
                  xlab = xlab, ylab = ylab)
  graphics::axis(1)
  graphics::axis(2)
  graphics::box()
  
  if(show_legend){
    if(is.null(legend_text))
      legend_text <- paste0("algorytm ", 1:num_of_algorithms)
    
    graphics::legend("topleft", inset=.002,
                     legend = legend_text,
                     col = line_colours, cex = legend_cex,
                     lwd = 4)
  }
  
  invisible(NULL)
}


append_the_list <- function(initial_list, additional_lists){
  out_list <- list()
  for(additional_list in additional_lists){
    out_list[[length(out_list) + 1]] <- additional_list
  }
  for(initial_list_single in initial_list){
    out_list[[length(out_list) + 1]] <- initial_list_single
  }
  
  out_list
}

plot_ecdf_list <- function(list_of_lists_f_vals, legent_additional_text = "", experiment = "1", ...){
  # values from parameter_tuning_generate_data.R:
  if(experiment == "0"){
    f_val_max <- -28.5     # my_goal_function(perm_real)
    f_val_id <- -45.8      # my_goal_function(permutations::id)
    f_val_med <- -60       # median(mc_list[[1]])
  }else if(experiment == "1"){
    f_val_max <- 173.8259  # my_goal_function(perm_real)
    f_val_id <- 79.51006   # my_goal_function(permutations::id)
    f_val_med <- -1.023756 # median(mc_list[[1]])
  }else if(experiment == "2"){
    f_val_max <- 56.0005   # my_goal_function(perm_real)
    f_val_id <- -108.26    # my_goal_function(permutations::id)
    f_val_med <- -241.3799 # median(mc_list[[1]])
  }else if(experiment == "3"){
    f_val_max <- -21.59946 # my_goal_function(perm_real)
    f_val_id <- -194.468   # my_goal_function(permutations::id)
    f_val_med <- -238.6469 # median(mc_list[[1]])
  }else{
    stop("Wrong experiment selected!")
  }
  
  plot_ecdf(list_of_lists_f_vals, min_val = f_val_med, max_val = f_val_max, reference_line = f_val_id,
            legend_text = c("MH", "MC", "BG_id", legent_additional_text), ...)
}

# `plot_ecdf_list_single` dostaje listę wyników, a nie listę list wyników
plot_ecdf_list_single <- function(list_of_f_vals, ...){
  my_list <- list()
  
  for(line in list_of_f_vals){
    my_list[[length(my_list) + 1]] <- list(line)
  }
  
  plot_ecdf_list(my_list, show_legend=FALSE, ...)
}


#' Mozna testowac `pop_size`, `success_treshold`, `a`, `k_max`, `tournament_part`, `init`
get_list_of_lists_of_log_values <- function(goal_function, pop_size, success_treshold, a,
                                            k_max, tournament_part, M, max_iter, init="random",
                                            max_f_calls=Inf, print_progress = TRUE){
  if(print_progress){
    start_time <- Sys.time()
  }
  
  stopifnot(sum(c(length(pop_size) > 1,
                  length(success_treshold) > 1,
                  length(a) > 1,
                  length(k_max) > 1,
                  length(tournament_part) > 1,
                  length(init) > 1)) == 1)
  
  list_of_lists_of_log_values <- list()
  
  if(length(pop_size) > 1){
    number_of_loops <- length(pop_size)
  }else if(length(success_treshold) > 1){
    number_of_loops <- length(success_treshold)
  }else if(length(a) > 1){
    number_of_loops <- length(a)
  }else if(length(k_max) > 1){
    number_of_loops <- length(k_max)
  }else if(length(tournament_part) > 1){
    number_of_loops <- length(tournament_part)
  }else if(length(init) > 1){
    number_of_loops <- length(init)
  }
  
  progressBar_iterations <- number_of_loops * M
  if(print_progress){
    progressBar <- utils::txtProgressBar(initial = 1, min = 0,
                                         max = progressBar_iterations)
  }
  
  for(i in 1:number_of_loops){
    list_of_log_values <- list()
    for(j in 1:M){
      if(print_progress){
        utils::setTxtProgressBar(progressBar, (i-1)*M + j)
      }
      
      pop_size_i <- pop_size
      success_treshold_i <- success_treshold
      a_i <- a
      k_max_i <- k_max
      tournament_part_i <- tournament_part
      init_i <- init
      
      if(length(pop_size) > 1){
        pop_size_i <- pop_size[i]
      }else if(length(success_treshold) > 1){
        success_treshold_i <- success_treshold[i]
      }else if(length(a) > 1){
        a_i <- a[i]
      }else if(length(k_max) > 1){
        k_max_i <- k_max[i]
      }else if(length(tournament_part) > 1){
        tournament_part_i <- tournament_part[i]
      }else if(length(init) > 1){
        init_i <- init[i]
      }
      
      eo <- evolutional_optimization(goal_function, max_iter=max_iter, pop_size=pop_size_i,
                                     success_treshold=success_treshold_i, p_0=0.5,
                                     a=a_i, k_max=k_max_i,
                                     tournament_part=tournament_part_i,
                                     init=init_i,
                                     max_f_calls = max_f_calls,
                                     show_progress_bar=FALSE)
        
      list_of_log_values[[j]] <- eo[["goal_function_logvalues"]]
    }
    list_of_lists_of_log_values[[i]] <- list_of_log_values
  }
  if(print_progress){
    close(progressBar)
    
    end_time <- Sys.time()
    
    print(end_time - start_time)
  }
  
  list_of_lists_of_log_values
}







get_list_of_log_values_MH <- function(goal_function, max_iter, M, print_progress = TRUE){
  if(print_progress){
    start_time <- Sys.time()
  }
  
  list_of_lists_of_log_values <- list()
  
  if(print_progress){
    progressBar <- utils::txtProgressBar(initial = 1, min = 0,
                                         max = M)
  }
  
  list_of_log_values <- list()
  for(j in 1:M){
    if(print_progress){
      utils::setTxtProgressBar(progressBar, j)
    }
    
    mh <- MH(attr(goal_function, "U"), n_number = attr(goal_function, "n"), max_iter = max_iter,
             show_progress_bar = FALSE)
    
    list_of_log_values[[j]] <- mh[["goal_function_logvalues"]]
  }
  
  if(print_progress){
    close(progressBar)
    
    end_time <- Sys.time()
    
    print(end_time - start_time)
  }
  
  list_of_log_values
}



get_list_of_log_values_MC <- function(goal_function, max_iter, M, print_progress = TRUE){
  if(print_progress){
    start_time <- Sys.time()
  }
  
  list_of_lists_of_log_values <- list()
  
  if(print_progress){
    progressBar <- utils::txtProgressBar(initial = 1, min = 0,
                                         max = M)
  }
  
  perm_size <- dim(attr(goal_function, "U"))[1]
  
  list_of_log_values <- list()
  for(j in 1:M){
    if(print_progress){
      utils::setTxtProgressBar(progressBar, j)
    }
    
    list_of_log_values[[j]] <- sapply(1:max_iter, function(i){goal_function(as.cycle(rperm(1, perm_size)))})
  }
  
  if(print_progress){
    close(progressBar)
    
    end_time <- Sys.time()
    
    print(end_time - start_time)
  }
  
  list_of_log_values
}


get_list_of_log_values_BG <- function(goal_function, max_iter, M, print_progress = TRUE){
  if(print_progress){
    start_time <- Sys.time()
  }
  
  list_of_lists_of_log_values <- list()
  
  if(print_progress){
    progressBar <- utils::txtProgressBar(initial = 1, min = 0,
                                         max = M)
  }
  
  list_of_log_values <- list()
  for(j in 1:M){
    if(print_progress){
      utils::setTxtProgressBar(progressBar, j)
    }
    
    suppressWarnings({
      bg <- best_growth(attr(goal_function, "U"), n_number = attr(goal_function, "n"), max_iter = max_iter,
                        start_perm = as.cycle(rperm(1, perm_size)), show_progress_bar = FALSE)
    })
    
    if(!bg[["did_converge"]]){
      warning(paste0("In iteration ", j, " the BG algorithm did not converge in max_iter = ",
                     max_iter, " iterations. Try with bigger max_iter."))
    }
    
    list_of_log_values[[j]] <- bg[["goal_function_logvalues"]]
  }
  
  if(print_progress){
    close(progressBar)
    
    end_time <- Sys.time()
    
    print(end_time - start_time)
  }
  
  list_of_log_values
}


make_BG_mean <- function(bg_list){
  max_bg_length <- 1:length(bg_list) %>% sapply(function(i){
    length(bg_list[[i]])
  }) %>% max
  
  for(i in 1:length(bg_list)){
    bg_list[[i]][length(bg_list[[i]] + 1):max_bg_length] <- NA
  }
  
  bg_list_t <- purrr::transpose(bg_list) %>% simplify_all()
  
  mean_bg_rand <- sapply(1:max_bg_length, function(i){
    mean(bg_list_t[[i]], na.rm=TRUE)
  }) %>% as_vector() %>% list()
}



