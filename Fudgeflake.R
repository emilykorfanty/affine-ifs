#========================================================
# Deterministic Algorithm Example: Fudgeflake
#========================================================


 library(ggplot2)

  # = Pick the number of iterations =
  N <- 4
  
  lambda <- 1/sqrt(3)
  theta <- pi/6
  R <- matrix(data=c(cos(theta), -sin(theta), sin(theta), cos(theta)), byrow=TRUE, nrow=2, ncol=2)
  
  A <- lambda*R
  
  f_1 <- function(x){
    A%*%x
  }
  
  f_2 <- function(x){
    A%*%x + c(1/2, sqrt(3)/6)
  }
  
  f_3 <- function(x){
    A%*%x + c(1/2, -sqrt(3)/6)
  }
  
  
  # = = = = = = = = = = = = 
  
  # = Define the starting set =
  
  a <- c(0,sqrt(3)/3)
  b <- c(1/2, sqrt(3)/3 + sqrt(3)/6)
  c <- c(1,sqrt(3)/3)
  d <- c(1,0)
  e <- c(1/2,-sqrt(3)/6)
  f <- c(0,0)
  
  S<-list()
  S[[1]] <- a
  S[[2]] <- b
  S[[3]] <- c
  S[[4]] <- d
  S[[5]] <- e
  S[[6]] <- f
  
  ref <- c(1/2, sqrt(3)/6)
  
  # = = = = = = = = = = = = = = = = = = = = = = = = =
  
  
  # = Iterate the functions =
  
  FF <- list()
  pts <- list()
  refs <- list()
  
  refs[[1]] <- ref
  
  for(i in 1:N){
    
    for(j in 1:length(refs)){
      
      pts[[j]] <- list(f_1(refs[[j]]),
                       f_2(refs[[j]]),
                       f_3(refs[[j]]))
    }
    
    for(j in 1:length(S)){
      
      FF[[j]] <- list(f_1(S[[j]]),
                      f_2(S[[j]]),
                      f_3(S[[j]]))
      
    }
    
    S <- unlist(FF, recursive = FALSE)
    refs <- unlist(pts, recursive=FALSE)
  }

  
# = Prep to plot the lines between the pairs of points in the list S
 
  X<-vector()
  Y<-vector()
  
  for(j in 1:length(S)){
   p <- S[[j]]
    X[j] <- p[1]
    Y[j] <- p[2]
  
 } 
  
# Prepare reference x and y values
  x<-vector()
  y<-vector()
  
  for(j in 1:length(refs)){
    r <- refs[[j]]
    x[j] <- r[1]
    y[j] <- r[2]
  } 

#----------------------------------------------------------
# Prepare a list of vertices to use as the shading polygons

refs_df <- data.frame(x,y)
pts_df <- data.frame(X, Y)
regions <- list()
corners <- list()

shrink <- lambda^N

 for(i in 1:length(refs)){
   for(j in 1: nrow(pts_df)){
     how_far <- c(sqrt((pts_df[[j,1]]-refs_df[[i,1]])^2 + (pts_df[[j,2]]-refs_df[[i,2]])^2))
     if(how_far <= shrink){
       corners[[j]] <- pts_df[j,]
     } else{corners[[j]] <- NA}
   }
   regions[[i]]<- corners
 }

#-----------------------------------------------
# Convert each list of vertices to a dataframe

regions_dfs <- list()

for(j in 1:length(regions)){
  
  test_region <- regions[[j]]
  tr_df <- data.frame(matrix(NA, nrow = length(test_region), ncol = 2))
  
  for(i in 1:length(test_region)){
    if(length(test_region[[i]][1])>0 & length(test_region[[i]][2])>0){
      
      tr_df[i,1] <- test_region[[i]][1]
      tr_df[i,2] <- test_region[[i]][2]
      
    }
  }
  
  # Remove null rows
  tr_df <- tr_df[unique(which(!is.na(tr_df), arr.ind = TRUE)[,1]),]
  
  # Remove duplicates
  tr_df <- tr_df[!duplicated(round(tr_df, digits=4)), ]
  
  # Add to list of dataframes
  regions_dfs[[j]] <- tr_df
}

#---------------------------------------------------------------------------------
# Plot the shading using geom_polygon
# When using geom_polygon, you will typically need two data frames:
# one contains the coordinates of each polygon (positions),  and the
# other the values associated with each polygon (values).  An id
# variable links the two together

# Combine regions into one dataframe, using convex hull to fix ordering
df <- regions_dfs[[1]]
df <- df[chull(df),]

for(i in 2:length(regions_dfs)){
  add_below <- regions_dfs[[i]]
  df <- rbind(df, add_below[chull(add_below),])
}
row.names(df) <- c(1:nrow(df))

# Define id variables to specify groups to be shaded
ids <- c(1:length(regions_dfs))

# Combine a ids, a constant shading value, and vertices
datapoly <- data.frame(
  id = rep(ids, each = 6),
  value = rep(1, each = 6),
  x = df$X1,
  y = df$X2
)

# Create the plot
p <- ggplot(datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id))
p + guides(fill=FALSE) +
    scale_fill_gradient(low="black", high="black") +
    theme(aspect.ratio=1) + xlab("") + ylab("")


