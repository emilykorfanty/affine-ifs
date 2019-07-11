# A Square-Type Fractal - Random Iteration Method
#-----------------------------------------
#-----------------------------------------
# IFS:
# f_i(x) = \lambda*x + v_i, i = 1, 2, 3, 4
# v_i are the following points:
# (1,1), (4, 1), (1, 4), (4, 4)
#
# Setup 
#-----------------------------------------
N <- 100000  # Number of iterations

# Vertices - change these for interesting images
V1 <- c(1,1)
V2 <- c(4, 1)
V3 <- c(1, 4)
V4 <- c(4, 4)
V <- list(V1, V2, V3, V4) 

# Decisions for which function to use in each iteration 
# P(v_i) = 1/8 for 1 = 1, ..., 8
D <- sample(c(1,2,3, 4),N,replace = TRUE)

X <- list() # A list to hold the points of the gasket

# Starting point
x <- 2
y <- 2
X[[1]] <- c(x,y)

# Apply the functions, in the order of D, to (x,y)
for(n in 2:N){
  X[[n]] <- c((1/2)*X[[n-1]] + V[[D[n]]])
}

# Prepare for plotting
M<- c(x,y)
for(i in 2:N){
  M <- rbind(M,X[[i]])  
}

# Plot the points
plot(x=M[,1],y=M[,2],
     xlab ="",ylab="",pch='.',frame.plot=FALSE)
#lines(c(0,sqrt(3/2)),c(1,-0.5),type = "l",col="blue",lwd=3)
#lines(c(0,-sqrt(3/2)),c(1,-0.5),type = "l",col="blue",lwd=3)
#lines(c(-sqrt(3/2),sqrt(3/2)),c(-0.5,-0.5),type = "l",col="blue",lwd=3)
#text(-0.125,1.125,paste0("N = ",N))
