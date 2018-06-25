rna <- function(z) { 
  y <- c(NA, head(z, -1))
  z <- ifelse(is.na(z), y, z)
  if (any(is.na(z))) {Recall(z)} else {z}}