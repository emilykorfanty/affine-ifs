#=====================
# Sierpinski Gasket
#=====================

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

  # = Pick the number of iterations =
  N <- 7

  # = Define the functions =
  f_1 <- function(x){
    (1/2)*x
  }
  
  f_2 <- function(x){
    (1/2)*x + c(1/2,0)
  }
  
  f_3 <- function(x){
    (1/2)*x + c(1/4,sqrt(3)/4)
  }
  
  # = Define the starting set as a list of pairs of points =
  
  A <- c(0,0)
  B <- c(1,0)
  C <- c(1/2, sqrt(3)/2)
  
  AB<-list()
  AB[[1]]<-A
  AB[[2]]<-B
  
  BC<-list()
  BC[[1]]<-B
  BC[[2]]<-C
  
  CA<-list()
  CA[[1]]<-C
  CA[[2]]<-A
  
  S<-list()
  S[[1]]<-AB
  S[[2]]<-BC
  S[[3]]<-CA
  
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
  
  # = Plot the lines between the pairs of points in the list S
 
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
  

  
  plot(c(X_1[1], X_2[1]), c(Y_1[1], Y_2[1]), type="l", xlim = c(0,1), ylim=c(0,sqrt(3)/2), asp=1, xlab="", ylab="", axes=FALSE)
  
 for(j in 2:length(S)){
   lines(c(X_1[j], X_2[j]), c(Y_1[j], Y_2[j]))
 }
  
  
  












