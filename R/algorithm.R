#' W mutacji liczba transpozycji będzie losowana z rozkladu binomialnego
evolutional_optimization <- function(U, n_number, max_iter, pop_size=100,
                                     mutation_factor=0.8, tournament_size=2,
                                     perm_size=NULL, delta=3, D_matrix=NULL,
                                     use_progress_bar=TRUE){
  if(is.null(perm_size)){
    perm_size <- dim(U)[1]
  }
  stopifnot(perm_size == dim(U)[1])
  if(is.null(D_matrix)){
    D_matrix <- diag(nrow = perm_size)
  }
  
  population <- list()
  for(i in 1:pop_size){
    population[[i]] <-  runif_perm(perm_size)
  }
  
  # TODO
}
