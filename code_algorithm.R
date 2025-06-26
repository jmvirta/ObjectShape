#-----------------------------------
# Object shape and the peeling plot
# JV



# Object shape

# D = distance matrix
# is.squared = whether the elements of D are squared distances or not
oshape <- function(D, is.squared = FALSE){
  if(!is.squared){
    D <- D^2
  }
  n <- nrow(D)
  sum(D%*%D)/(n*sum(D^2))
}





# Peeling estimator

# D = distance matrix (with elements being non-squared distances)
# min = TRUE/FALSE, whether we want to minimize (find lines) or maximize (find sphericity)
peel <- function(D, min = TRUE){
  n <- nrow(D)
  D <- D^2
  
  res <- matrix(0, n, 2)
  
  list_of_elem <- 1:n
  
  removed_elem <- NULL
  
  n_tail <- floor(0.1*n)
  
  for(i in 1:(n - n_tail)){
    
    remaining_elem <- setdiff(list_of_elem, removed_elem)
    
    # length(remaining_elem) = n - i + 1
    
    temp <- cbind(remaining_elem, rep(0, n - i + 1))
    for(j in 1:(n - i + 1)){
      temp[j, 2] <- oshape(D[remaining_elem[-j], remaining_elem[-j]], is.squared = TRUE)
    }
    
    if(min){
      min_ind <- which.min(temp[, 2])[1]
    }
    if(!min){
      min_ind <- which.max(temp[, 2])[1]
    }
    
    res[i, ] <- temp[min_ind, ] 
    removed_elem <- c(removed_elem, remaining_elem[min_ind])
    
  }
  
  remaining_elem <- setdiff(list_of_elem, removed_elem)
  
  for(j in 0:(n_tail - 1)){
    res[n - j, ] <- c(remaining_elem[j + 1], res[n - n_tail, 2])
  }
  
  
  res
}



# Peeling plot
#
# peel_obj = an object returned by the function peel()
peel_plot <- function(peel_obj){
  n <- nrow(peel_obj)
  plot(1:n, peel_obj[, 2], type = "l")
}
