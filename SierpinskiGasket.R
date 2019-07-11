# Sierpinski Gasket - Random Iteration Method
#-----------------------------------------
#-----------------------------------------
# IFS:
# f_i(x) = 0.5(x+v_i), i = 1, 2, 3
# v_i are the vertices of the triangle
#
# Setup 
#-----------------------------------------
N <- 10000  # Number of iterations

# Triangle ABC
A <- c(0,1)
B <- c(sqrt(3/2),-0.5)
C <- c(-sqrt(3/2),-0.5)
V <- list(A,B,C) 

# Decisions for which function to use in each iteration 
# P(A) = P(B) = P(C) = 1/3
D <- sample(c(1,2,3),N,replace = TRUE)

X <- list() # A list to hold the points of the gasket

# Starting point
x <- 0
y <- 1
X[[1]] <- c(x,y)

# Apply the functions, in the order of D, to (x,y)
for(n in 2:N){
  X[[n]] <- c(0.5*(V[[D[n]]]+X[[n-1]]))
}

# Prepare for plotting
M<- c(x,y)
for(i in 2:N){
  M <- rbind(M,X[[i]])  
}

# Plot the points
plot(x=M[,1],y=M[,2],xlim = c(-1.5,1.5),ylim = c(-1,1),
     xlab ="",ylab="",pch='.',frame.plot=FALSE)
lines(c(0,sqrt(3/2)),c(1,-0.5),type = "l",col="blue",lwd=3)
lines(c(0,-sqrt(3/2)),c(1,-0.5),type = "l",col="blue",lwd=3)
lines(c(-sqrt(3/2),sqrt(3/2)),c(-0.5,-0.5),type = "l",col="blue",lwd=3)
text(-1,0.8,paste0("N = ",N))
