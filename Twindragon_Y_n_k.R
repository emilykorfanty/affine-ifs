#========================================================
# Deterministic Algorithm Example: Fudgeflake
# Calculating Y_{n,k} complements
#========================================================

# This algorithm iterates functions on a starting set to create the Sierpinski Gasket. 
#
# The starting set is defined as a LIST of PAIRS of points in the plane.
#
# Each PAIR of points corresponds to a straight line drawn between them.
#
# The functions are applied to each of the endpoints, creating a new list of pairs of points.
#
# This process is repeated for the specified number of iterations.
#
# Finally, the resulting set is plotted by drawing lines between the pairs of points.
#

library(ggplot2)
library(reshape2)

setwd("C:/Users/Emily/Documents/GitHub/affine-ifs")

source("functions.R")

# = Pick the number of iterations =
N <- 1

digits <- 4

lambda <- 1/sqrt(2)
theta <- pi/4
R <- matrix(data=c(cos(theta), -sin(theta), sin(theta), cos(theta)), byrow=TRUE, nrow=2, ncol=2)

A <- lambda*R

f_1 <- function(x){
  A%*%x
}

f_2 <- function(x){
  A%*%x + c(1/2, -1/2)
}




# = = = = = = = = = = = = 

# = Define the starting set =

a <- c(0,0)
b <- c(1/2,-1/2)
c <- c(1/2,1/2)
d <- c(1,0)

S<-list()
S[[1]] <- a
S[[2]] <- b
S[[3]] <- d
S[[4]] <- c

ref <- c(1/2, 0)

# = = = = = = = = = = = = = = = = = = = = = = = = =


# = Iterate the functions =

FF <- list()
pts <- list()
refs <- list()

refs[[1]] <- ref

for(i in 1:N){
  
  for(j in 1:length(refs)){
    
    pts[[j]] <- list(f_1(refs[[j]]),
                     f_2(refs[[j]]))
  }
  
  for(j in 1:length(S)){
    
    FF[[j]] <- list(f_1(S[[j]]),
                    f_2(S[[j]]))
    
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
  tr_df <- tr_df[!duplicated(round(tr_df, digits=digits)), ]
  
  # Add to list of dataframes
  regions_dfs[[j]] <- tr_df
}

#---------------------------------------------------------------------------------
# Set up for plotting the shading using geom_polygon
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

#--------------------------------------------------------------------
# Apply the three functions to the resulting set of points
# Apply the pairwise compositions of these functions as well
#--------------------------------------------------------------------

# collect the three functions in a list
functions <- list(f_1, f_2)
# get the compositions of these functions
compositions <- compose(functions)
#compositions <- functions

# create a list of dataframes containing the result of applying
# each of these functions to df
results <- apply_functions(df, compositions)

# find the pairwise intersections of images
matches <- pair_intersect(results, digits)

#--------------------------------------------------------------------
# Plot the copies of the fudgeflake in different colours 
#--------------------------------------------------------------------

# defines the hexagons in the original fudgeflake
ids1 <-datapoly[['id']]
num_values <- length(ids1)

# creates new ids and values for the copies
# also, collect x and y values
num_ids <- length(unique(ids1))
#ids1 <- c(ids1, ids1 + num_ids, ids1 + 2*num_ids)
#values1 <- c(rep(1, each = num_values), rep(2, each = num_values), rep(3, each = num_values)) 

values1 <- vector()
x_values <- vector()
y_values <- vector()
new_ids <- list()
for(i in 1:length(compositions)){
  
  new_ids[[i]] <- c(ids1 + (i-1)*num_ids)
  
  values1 <- c(values1, rep(i, each=num_values))
  x_values <- c(x_values, results[[i]][[1]])
  y_values <- c(y_values, results[[i]][[2]])
}


ids1 <- unlist(new_ids)

datapoly1 <- data.frame(
  id = ids1,
  value = values1,
  x = x_values,
  y = y_values
)



# Create the plot
p <- ggplot(datapoly1, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id))
p + guides(fill=FALSE) +
  scale_fill_gradient(low="gray15", high="gray60") +
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
        plot.background=element_blank())+
  theme(aspect.ratio=1) 


#--------------------------------------------------------------------
# Find the intersections of the matches and plot them
#--------------------------------------------------------------------

# put the matches into the format: variable, x, y
x_df <- matches[[1]][1]
y_df <- matches[[1]][2]
for(i in 2:length(matches)){
  x_df <- cbind(x_df, matches[[i]][1])
  y_df <- cbind(y_df, matches[[i]][2])
}
colnames(x_df)<-c(1:ncol(x_df))
colnames(y_df)<-c(1:ncol(y_df))

x_df <- melt(x_df)
colnames(x_df)<- c("match", "x")

y_df <- melt(y_df)
colnames(y_df)<- c("match", "y")

xy_df <- cbind(x_df, y_df[2])


starts <- xy_df[duplicated(round(xy_df[2:3], digits=digits)), ]
starts <- starts[2:3]
starts <- starts[!duplicated(round(starts, digits=digits)), ]

#flake_color <- "grey"
#flake_color <- "azure3"
#flake_color <- "darkgray"
#flake_color <- "black"
#flake_color <- "dimgray"
flake_color <- "gray55"
#line_color <- "blue"
line_color <- "red3"


# Create the plot
p <- ggplot(datapoly1, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id))
p + guides(fill=FALSE) +
  scale_fill_gradient(low=flake_color, high=flake_color) +
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
        plot.background=element_blank())+
  theme(aspect.ratio=1) +
  geom_point(data = xy_df, aes(color = match)) +
  geom_point(data = starts) 


#-----------------------------------------------------------------------
# Assign match intersections to matches by distances (for line-drawing)
#-----------------------------------------------------------------------

match_starts <- list()
for(i in 1:length(matches)){
  min_dist <- Inf
  min_idx <- 0
  match <- matches[[i]]
  #assigned <- vector()
  for(j in 1:nrow(starts)){
    this_start <- as.numeric(starts[j,])
    for(k in 1:nrow(match)){
      row <- as.numeric(match[k,])
      dist <- sqrt((row[1] - this_start[1])^2 + (row[2] - this_start[2])^2 )
      if(dist < min_dist){
        min_dist <- dist
        min_idx <- j
      }
    }
    
  }
  if(round(min_dist, digits==digits) == 0){
    match_starts[[i]] <- as.numeric(starts[min_idx,])
  }
  
}

#--------------------------------------------------------------------
# Sort matches based on distance from associated starting point
# Convert list of lines into a dataframe
# Create an index for the lines and put into long form
#--------------------------------------------------------------------

lines <- list()
for(i in 1:length(matches)){
  lines[[i]] <- dist_sort(df = matches[[i]], start = match_starts[[i]], digits = 8)
}

# put the matches into the format: variable, x, y
lines_x <- lines[[1]][1]
lines_y <- lines[[1]][2]
for(i in 2:length(lines)){
  lines_x <- cbind(lines_x, lines[[i]][1])
  lines_y <- cbind(lines_y, lines[[i]][2])
}
colnames(lines_x)<-c(1:ncol(lines_x))
colnames(lines_y)<-c(1:ncol(lines_y))

lines_x <- melt(lines_x)
colnames(lines_x)<- c("line", "x")

lines_y <- melt(lines_y)
colnames(lines_y)<- c("line", "y")

lines_df <- cbind(lines_x, lines_y[2])

#--------------------------------------------------------------------
# Plot the intersections of images on top of the fudgeflake
#--------------------------------------------------------------------

#flake_color <- "grey"
#flake_color <- "azure3"
#flake_color <- "darkgray"
#flake_color <- "black"
#flake_color <- "dimgray"
flake_color <- "gray55"
#line_color <- "blue"
line_color <- "red3"


# Create the plot
p <- ggplot(datapoly1, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id))
p + guides(fill=FALSE) +
  scale_fill_gradient(low=flake_color, high=flake_color) +
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
        plot.background=element_blank())+
  theme(aspect.ratio=1) +
  geom_point(data = starts, color = line_color, size = 3)
#geom_path(data = lines[[1]], color = line_color, size = 1.5) +
#geom_path(data = lines[[2]], color = line_color, size = 1.5) +
#geom_path(data = lines[[3]], color = line_color, size = 1.5) #+
#geom_path(data = lines[[4]], color = line_color, size = 1.5) +
#geom_path(data = lines[[5]], color = line_color, size = 1.5) +
#geom_path(data = lines[[6]], color = line_color, size = 1.5) +
#geom_path(data = lines[[7]], color = line_color, size = 1.5) +
#geom_path(data = lines[[8]], color = line_color, size = 1.5) +
#geom_path(data = lines[[9]], color = line_color, size = 1.5) +
#geom_path(data = lines[[10]], color = line_color, size = 1.5) +
#geom_path(data = lines[[11]], color = line_color, size = 1.5) +
#geom_path(data = lines[[12]], color = line_color, size = 1.5) +
#geom_path(data = lines[[13]], color = line_color, size = 1.5) +
#geom_path(data = lines[[14]], color = line_color, size = 1.5) +
#geom_path(data = lines[[15]], color = line_color, size = 1.5) 


