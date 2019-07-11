# Sierpinski Carpet - Random Iteration Method
#-----------------------------------------
#-----------------------------------------
# IFS:
# f_i(x) = (1/3)*x + v_i, i = 1, 2, ..., 8
# v_i are the following points:
# (0,0), (0, 1/3), (0, 2/3), (1/3, 2/3), (2/3, 2/3), (2/3, 1/3), (2/3, 0), (1/3, 0)
#
# Setup 
#-----------------------------------------
N <- 70000  # Number of iterations

# Triangle ABC
V1 <- c(0,0)
V2 <- c(0, 1/3)
V3 <- c(0, 2/3)
V4 <- c(1/3, 2/3)
V5 <- c(2/3, 2/3)
V6 <- c(2/3, 1/3)
V7 <- c(2/3, 0)
V8 <- c(1/3, 0)
V <- list(V1, V2, V3, V4, V5, V6, V7, V8) 

# Decisions for which function to use in each iteration 
# P(v_i) = 1/8 for 1 = 1, ..., 8
D <- sample(c(1,2,3, 4, 5, 6, 7, 8),N,replace = TRUE)

X <- list() # A list to hold the points of the gasket

# Starting point
x <- 0
y <- 0
X[[1]] <- c(x,y)

# Apply the functions, in the order of D, to (x,y)
for(n in 2:N){
  X[[n]] <- c((1/3)*X[[n-1]] + V[[D[n]]])
}

# Prepare for plotting
M<- c(x,y)
for(i in 2:N){
  M <- rbind(M,X[[i]])  
}

# Plot the points
plot(x=M[,1],y=M[,2],xlim = c(-0.25,1.25),ylim = c(-0.25,1.25),
     xlab ="",ylab="",pch='.',frame.plot=FALSE)
#lines(c(0,sqrt(3/2)),c(1,-0.5),type = "l",col="blue",lwd=3)
#lines(c(0,-sqrt(3/2)),c(1,-0.5),type = "l",col="blue",lwd=3)
#lines(c(-sqrt(3/2),sqrt(3/2)),c(-0.5,-0.5),type = "l",col="blue",lwd=3)
text(-0.125,1.125,paste0("N = ",N))
