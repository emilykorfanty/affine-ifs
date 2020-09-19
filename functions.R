#-----------------------------------------------------------------------
# Sort the points in a dataframe with two numeric columns 
# Given a starting point, order by next closest point
# The starting point should be one of the rows of the dataframe
# Digits is a rounding tolerance for equality of points
#-----------------------------------------------------------------------

dist_sort <- function(df, start, digits){
  


# Sort based on distance from the center
new_x <- start[1]
new_y <- start[2]
current <- start
assigned <- vector()
idx <- 1

while(idx <= nrow(df)){
  dists <- vector()
  dists_idx <- vector()
  # Find the distances from current to rows not assigned yet
  for(i in 1:nrow(df)){
    row <- round(as.numeric(df[i,]), digits=digits)
    if(!(i %in% assigned)){
      dist <- sqrt((row[1] - current[1])^2 + (row[2] - current[2])^2 )
      if(dist > 0){
        dists <- c(dists, dist)
        dists_idx <- c(dists_idx, i)
      }
    }
  }
  # Find the closest of these points
  min_dist <- min(dists)
  closest_idx <- dists_idx[which(dists==min_dist)]
  closest <- as.numeric(df[closest_idx,])
  
  # Update current
  current <- closest
  
  # Update assigned 
  assigned[idx] <- closest_idx
  
  # Save the new current point
  new_x <- c(new_x, current[1])
  new_y <- c(new_y, current[2])
  
  # Increase the count
  idx <- idx + 1

}
data.frame(x=new_x, y=new_y)

}

#-----------------------------------------------------------------------
# Given a list of functions, create a new list of functions 
# Containing every possible composition of these functions
# The number of functions to be composed is specified as n
# Digits is a rounding tolerance for equality of points
# All functions must be of a single variable
#-----------------------------------------------------------------------

compose <- function(functions){
  composed <- list()
  m <- length(functions)
  count <- 1
  for(i in 1:m){
    for(j in 1:m){
      composed[[count]] <- eval(substitute(function(x){
        functions[[i]](functions[[j]](x))
      }, list(i = i, j = j)))
      count <- count+1
    }
  }
  composed
}

#-----------------------------------------------------------------------
# Apply a list of functions to a dataframe
# Collect the results into list of dataframes
# The original dataframe must have two columns
#-----------------------------------------------------------------------

apply_functions <- function(df, functions){
  results <- list()
  m <- length(functions)

  for(i in 1:m){
    f <- functions[[i]]
    result <- mapply(function(x1, x2) f(c(x1, x2)), df[[1]], df[[2]])
    results[[i]] <- data.frame(f_X1 = c(result[1,]), f_X2 = c(result[2,]))
  }
  results
}


#-----------------------------------------------------------------------
# Given a list of dataframes, find the pairwise intersections
# of the values
# The original dataframes must have two columns
# Digits is a rounding tolerance for equality of points
#-----------------------------------------------------------------------

pair_intersect <- function(dfs, digits){
  intsns <- list()
  m <- length(dfs)
  count <- 1
  compared <- vector()
  for(i in 1:(m-1)){
    A <- dfs[[i]]
    for(j in 1:m){
      if(j > i){
        #if(!(j %in% compared)){
          intsn_x <- list()
          intsn_y <- list()
          B <- dfs[[j]]
        
          for(k in 1:nrow(A)){
            for(l in 1:nrow(B)){
              # check if the point i and j are equal
              p1 <- c(A[k,1], A[k,2])
              p2 <- c(B[l,1], B[l,2])
              if(all(round(p1, digits=digits) == round(p2, digits=digits))){
                intsn_x <- append(intsn_x, p1[1])
                intsn_y <- append(intsn_y, p1[2])
              }
            #}
          }
        }
        
        intsn_x <- unlist(intsn_x)
        intsn_y <- unlist(intsn_y)
        intsn <- data.frame(x = intsn_x, y = intsn_y)
        
        # Remove duplicates
        intsn <- intsn[!duplicated(round(intsn, digits=digits)), ]
        
        if(nrow(intsn)>0){
          #if(i<m-1){
            intsns[[count]]<- intsn
            count <- count + 1
          #}
        }
      }
    }
    compared <- c(compared, i)
  }
  intsns
}

#-----------------------------------------------------------------------
# Given a list of dataframes, find the intersections for each group of 
# three of them
# The original dataframes must have two columns
# Digits is a rounding tolerance for equality of points
#-----------------------------------------------------------------------

triple_intersect <- function(dfs, digits){
  intsns <- list()
  m <- length(dfs)
  count <- 1
  compared <- vector()
  for(i in 1:m-2){
    A <- dfs[[i]]
    checked <- vector()
    for(j in i:m-1){
      #if(!(j %in% compared)){
        B <- dfs[[j]]
        for(q in j:m){
          #if(!(q %in% checked)){
            C <- dfs[[q]]
            intsn_x <- list()
            intsn_y <- list()
            
            for(k in 1:nrow(A)){
                for(l in 1:nrow(B)){
                  for(r in 1:nrow(C)){
                    p1 <- c(A[k,1], A[k,2])
                    p2 <- c(B[l,1], B[l,2])
                    p3 <- c(C[r,1], C[r,2])
                    if(all(round(p1, digits=digits) == round(p2, digits=digits))){
                      if(all(round(p2, digits=digits) == round(p3, digits=digits))){
                        intsn_x <- append(intsn_x, p1[1])
                        intsn_y <- append(intsn_y, p1[2])
                      }
                    }
                  #}
                }
              #}
          }
        }
        
        intsn_x <- unlist(intsn_x)
        intsn_y <- unlist(intsn_y)
        intsn <- data.frame(x = intsn_x, y = intsn_y)
        #intsn <- c(x,y)
        # Remove duplicates
        intsn <- intsn[!duplicated(round(intsn, digits=digits)), ]
        
        if(nrow(intsn)>0){
            intsns[[count]]<- intsn
            count <- count + 1
           
        }
      }
      checked <- c(checked, j)
    }
    compared <- c(compared, i)
  }
  intsns
}


