### `reshape()` for "unbalanced" datasets ###
### https://gist.githubusercontent.com/mrdwab/6123681/raw/a1f91b00a4c0df2a9a96bfa5a3208a63930d79cf/uReshape.R ###

uReshape <- function(data, id.vars, var.stubs, sep) {
  # vectorized version of grep
  vGrep <- Vectorize(grep, "pattern", SIMPLIFY = FALSE)
  
  # Isolate the columns starting with the var.stubs
  temp <- names(data)[names(data) %in% unlist(vGrep(var.stubs, names(data), value = TRUE))]
  
  # Split the vector and reasemble into a data.frame
  x <- do.call(rbind.data.frame, strsplit(temp, split = sep))
  names(x) <- c("VAR", paste(".time", 1:(length(x)-1), sep = "_"))
  
  # Prep to decide whether normal reshape or unbalanced reshape
  xS <- split(x$.time_1, x$VAR)
  xL <- unique(unlist(xS))
  
  if (isTRUE(all(sapply(xS, function(x) all(xL %in% x))))) {
    # Everything looks ok for normal `reshape` to work
    reshape(data, direction = "long", idvar = id.vars, 
            varying = lapply(vGrep(var.stubs, names(data), value = TRUE), sort), 
            sep = sep, v.names = var.stubs)
  } else {
    # Padding required to "balance" the data
    
    # Find out which variables need to be padded
    newVars <- unlist(lapply(names(xS), function(y) {
      temp <- xL[!xL %in% xS[[y]]]
      if (length(temp) == 0) {
        temp <- NULL
      } else {
        paste(y, temp, sep = sep)
      }
    }))
    
    # Create matrix of NAs
    myMat <- setNames(data.frame(matrix(NA, nrow = nrow(data), ncol = length(newVars))), newVars)
    
    # Bind with original data.frame
    out <- cbind(data, myMat)
    
    # Use `reshape` as normal
    reshape(out, direction = "long", idvar = id.vars,
            varying = lapply(vGrep(var.stubs, names(out), 
                                   value = TRUE), sort), 
            sep = sep, v.names = var.stubs)
  }
}