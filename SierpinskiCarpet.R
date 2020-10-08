#========================================================
# Deterministic Algorithm Example: Sierpinski Carpet
#========================================================


library(ggplot2)

# = Pick the number of iterations =
N <- 3

lambda <- 1/3

# = Define the functions =
theta <- 0
R <- matrix(data=c(cos(theta), -sin(theta), sin(theta), cos(theta)), byrow=TRUE, nrow=2, ncol=2)

A <- (1/3)*R

f_1 <- function(x){
  A%*%x + c(0,0)
}

f_2 <- function(x){
  A%*%x + c(0,1/3)
}

f_3 <- function(x){
  A%*%x + c(0,2/3)
}

f_4 <- function(x){
  A%*%x + c(1/3,0)
}

f_5 <- function(x){
  A%*%x + c(1/3,2/3)
}

f_6 <- function(x){
  A%*%x + c(2/3,0)
}

f_7 <- function(x){
  A%*%x + c(2/3,1/3)
}

f_8 <- function(x){
  A%*%x + c(2/3,2/3)
}

# = = = = = = = = = = = = 

# = Define the starting set =

a <- c(0, 0)
b <- c(1, 0)
c <- c(1, 1)
d <- c(0, 1)

S<-list()
S[[1]] <- a
S[[2]] <- b
S[[3]] <- c
S[[4]] <- d

ref <- c(1/2, 1/2)

# Plot the starting set
# = Prep to plot the lines between the pairs of points in the list S

X1<-vector()
X2<-vector()

for(j in 1:length(S)){
  p <- S[[j]]
  X1[j] <- p[1]
  X2[j] <- p[2]
  
} 

# Combine regions into one dataframe, using convex hull to fix ordering
df <- data.frame(X1 = X1, X2 = X2)

# Define id variables to specify groups to be shaded
ids <- c(1)

# Combine a ids, a constant shading value, and vertices
datapoly <- data.frame(
  id = rep(ids, each = 4),
  value = rep(1, each = 4),
  x = df$X1,
  y = df$X2
)

# Create the plot
# p <- ggplot(datapoly, aes(x = x, y = y)) +
#   geom_polygon(aes(fill = value, group = id))
# p + guides(fill=FALSE) +
#   scale_fill_gradient(low="black", high="black") +
#   theme(aspect.ratio=1) + xlab("") + ylab("") + theme(aspect.ratio=1) +
#   theme(axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position="none",
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background=element_blank())



# = = = = = = = = = = = = = = = = = = = = = = = = =


# = Iterate the functions =

FF <- list()
pts <- list()
refs <- list()

refs[[1]] <- ref

for(i in 1:N){
  
  for(j in 1:length(refs)){
    this_ref <- unlist(refs[[j]])
    pts[[j]] <- list(f_1(refs[[j]]),
                     f_2(refs[[j]]),
                     f_3(refs[[j]]),
                     f_4(refs[[j]]),
                     f_5(refs[[j]]),
                     f_6(refs[[j]]),
                     f_7(refs[[j]]),
                     f_8(refs[[j]]))
  }
  
  for(j in 1:length(S)){
    
    FF[[j]] <- list(f_1(S[[j]]),
                    f_2(S[[j]]),
                    f_3(S[[j]]),
                    f_4(S[[j]]),
                    f_5(S[[j]]),
                    f_6(S[[j]]),
                    f_7(S[[j]]),
                    f_8(S[[j]]))
    
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
  id = rep(ids, each = 4),
  value = rep(1, each = 4),
  x = df$X1,
  y = df$X2
)

# Create the plot
p <- ggplot(datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id))
p + guides(fill=FALSE) +
  scale_fill_gradient(low="black", high="black") +
  theme(aspect.ratio=1) + xlab("") + ylab("")


# Create the plot
p <- ggplot(datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id))
p + guides(fill=FALSE) +
  scale_fill_gradient(low="black", high="black") +
  theme(aspect.ratio=1) + xlab("") + ylab("") + theme(aspect.ratio=1) + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())