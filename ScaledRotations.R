# With some rotations - Random Iteration Method
#-----------------------------------------
#-----------------------------------------
# IFS:
# f_i(x) = \lambda R*x + v_i, i = 1, 2, 3, 4
# 
# Setup 
#-----------------------------------------
N <- 10000  # Number of iterations

# scale
lambda <- 0.3

# rotation 
theta <- pi/3
R <- matrix(data=c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2, ncol = 2, byrow = TRUE)

# shifts 
V1 <- c(0,0)
V2 <- c(0, 2)
V3 <- c(1, 0)
V4 <- c(1, 2)
V <- list(V1, V2, V3, V4) 



# Decisions for which function to use in each iteration 
# P(v_i) = 1/8 for 1 = 1, ..., 8
D <- sample(c(1,2,3, 4),N,replace = TRUE)

X <- list() # A list to hold the points of the gasket

# Starting point
x <- 1
y <- 1
X[[1]] <- c(x,y)

# Apply the functions, in the order of D, to (x,y)
for(n in 2:N){
  X[[n]] <- c(lambda*R%*%X[[n-1]] + V[[D[n]]])
}

# Prepare for plotting
M<- c(x,y)
for(i in 2:N){
  M <- rbind(M,X[[i]])  
}

# Plot the points
plot(x=M[,1],y=M[,2],
     xlab ="",ylab="",pch='.',frame.plot=FALSE)
