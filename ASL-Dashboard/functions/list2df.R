
list2df <- function(list){
  
  x <- as_tibble(do.call(rbind, lapply(lapply(list, unlist), "[", unique(unlist(c(sapply(list,names)))))))
  return(x)
  
}

