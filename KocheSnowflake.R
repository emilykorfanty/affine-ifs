# Koche Snowflake - Random Iteration Method
# 
#-----------------------------------------
#-----------------------------------------
# IFS:
# f_i(x) = \lambda_i R_i*x + v_i, i = 1, ..., 7
# 
# Setup 
#-----------------------------------------
N <- 100000  # Number of iterations

# scale
lambda <- list(1/sqrt(3), 1/3, 1/3, 1/3, 1/3, 1/3, 1/3)

# rotation angles
theta <- list(pi/6, 0, 0, 0, 0, 0, 0)  

# rotation matrices 
R <- list()
for(i in 1:length(theta)){
  R[[i]] <- matrix(data=c(cos(theta[[i]]), -sin(theta[[i]]), sin(theta[[i]]), cos(theta[[i]])), nrow = 2, ncol = 2, byrow = TRUE)
}

# shifts 
V <- list(c(0, 0), c(1/sqrt(3), 1/3), c(0, 2/3), c(-1/sqrt(3), 1/3), c(-1/sqrt(3), -1/3), c(0, -2/3), c(1/sqrt(3), -1/3)) 

# the ifs, FF(i, x) = f_i(x)
FF <- function(i, input_vector){
  output_vector <- c(unlist(lambda[[i]])*unlist(R[[i]])%*%input_vector + unlist(V[[i]]))
  return(output_vector)}

# Decisions for which function to use in each iteration 
# P(v_i) = 1/7 for 1 = 1, ..., 7
D <- sample(c(1,2,3,4,5,6,7),N,replace = TRUE)

X <- list() # A list to holMd the points of the gasket

# Starting point
x_0 <- 0
y_0 <- 0
X[[1]] <- c(x_0,y_0)

# Apply the functions, in the order of D, to (x_0, y_0)
for(n in 2:N){
  X[[n]] <- FF(unlist(D[[n-1]]), unlist(X[[n-1]]))
}

# Prepare for plotting
M<- c(x_0,y_0)
for(i in 2:N){
  M <- rbind(M,X[[i]])  
}

# Plot the points
plot(x=M[,1],y=M[,2],
     xlab ="",ylab="",pch='.',frame.plot=FALSE)

