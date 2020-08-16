#========================================================
# Deterministic Algorithm Example: Sierpinski Gasket
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

N <- 6

theta <- pi/6
R <- matrix(data=c(cos(theta), -sin(theta), sin(theta), cos(theta)), byrow=TRUE, nrow=2, ncol=2)

A <- (1/sqrt(3))*R

f_1 <- function(x){
  A%*%x
}

f_2 <- function(x){
  A%*%x + c(1/2, sqrt(3)/6)
}

f_3 <- function(x){
  A%*%x + c(1/2, -sqrt(3)/6)
}


F_1 <- function(X){
  Y <- list()
  Y[[1]]<-f_1(X[[1]])
  Y[[2]]<-f_1(X[[2]])
  Y
}

F_2 <- function(X){
  Y <- list()
  Y[[1]]<-f_2(X[[1]])
  Y[[2]]<-f_2(X[[2]])
  Y
}

F_3 <- function(X){
  Y <- list()
  Y[[1]]<-f_3(X[[1]])
  Y[[2]]<-f_3(X[[2]])
  Y
  
}

# = = = = = = = = = = = = 

# = Define the starting set =

ab<-list()
ab[[1]]<-c(0,sqrt(3)/3)
ab[[2]]<-c(1/2, sqrt(3)/3 + sqrt(3)/6)

bc<-list()
bc[[1]]<-c(1/2, sqrt(3)/3 + sqrt(3)/6)
bc[[2]]<-c(1,sqrt(3)/3)

cd<-list()
cd[[1]]<-c(1,sqrt(3)/3)
cd[[2]]<-c(1,0)

de<-list()
de[[1]]<-c(1,0)
de[[2]]<-c(1/2,-sqrt(3)/6)


ef<-list()
ef[[1]]<-c(1/2,-sqrt(3)/6)
ef[[2]]<-c(0,0)

fa<-list()
fa[[1]]<-c(0,0)
fa[[2]]<-c(0,sqrt(3)/3)

S<-list()
S[[1]] <- ab
S[[2]] <- bc
S[[3]] <- cd
S[[4]] <- de
S[[5]] <- ef
S[[6]] <- fa



# = = = = = = = = = = = = = = = = = = = = = = = = =


# = Define the functions on lists of two elements =

F_1 <- function(X){
  Y <- list()
  Y[[1]]<-f_1(X[[1]])
  Y[[2]]<-f_1(X[[2]])
  Y
}

F_2 <- function(X){
  Y <- list()
  Y[[1]]<-f_2(X[[1]])
  Y[[2]]<-f_2(X[[2]])
  Y
}

F_3 <- function(X){
  Y <- list()
  Y[[1]]<-f_3(X[[1]])
  Y[[2]]<-f_3(X[[2]])
  Y
}

# = = = = = = = = = = = = 



# = Iterate the functions =

FF <- list()

for(i in 1:N){
  
  for(j in 1:length(S)){
    
    FF[[j]] <- list(F_1(S[[j]]),
                    F_2(S[[j]]),
                    F_3(S[[j]]))
    
  }
  
  S <- unlist(FF, recursive = FALSE)
}


# = Prep to plot the lines between the pairs of points in the list S

X_1<-vector()
Y_1<-vector()
X_2<-vector()
Y_2<-vector()

for(j in 1:length(S)){
  p <- S[[j]][[1]]
  X_1[j] <- p[1]
  Y_1[j] <- p[2]
  
  q <- S[[j]][[2]]
  X_2[j] <- q[1]
  Y_2[j] <- q[2]
  
} 

# Combine the x and y values into a data.frame with grouping
# to specify which points should be connected by a line
X <- c(X_1, X_2)
Y <- c(Y_1, Y_2)
ids <- c(1:length(S))
group <- rep(ids, times=2)
lines <- data.frame(x=X, y=Y, grp=group)

# Plot the lines
ggplot(lines, aes(x, y, group = grp)) + 
  geom_line() + xlab("") + ylab("") + theme(aspect.ratio=1)

# Plot the just lines  
#plot(c(X_1[1], X_2[1]), c(Y_1[1], Y_2[1]), type="l", xlim = c(0,1), ylim=c(0,sqrt(3)/2), asp=1, xlab="", ylab="", axes=FALSE)

#for(j in 2:length(S)){
#  lines(c(X_1[j], X_2[j]), c(Y_1[j], Y_2[j]))
#  }
