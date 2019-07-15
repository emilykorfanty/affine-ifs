# Koche Snowflake - Random Iteration Method
# 
#-----------------------------------------
#-----------------------------------------
# IFS:
# f_i(x) = \lambda_i R_i*x + v_i, i = 1, ..., m
# 
# Setup 
#-----------------------------------------
#-----------------------------------------
setwd("~/Emily/IFS Code")
data1 <- read.csv(file="ifs_data.csv",header = TRUE,stringsAsFactors = FALSE)

nr<-length(count.fields("ifs_data.csv", skip = 1))

data_fix <- data1[3:8]
for(i in 1:6){
  for(j in 1:nr){
  data_fix[i][j,1] <- sapply(data_fix[i][j,1], function(x) eval(parse(text=x)))}
}

data1[3:8]<-data_fix

# Input the ifs name, and number of iterations-----------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

#ifs <- "SierpinskiGasket"
ifs <- "SierpinskiCarpet"
#ifs <- "KochSnowflake"

# Number of iterations
N <- 20000

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------


k<-1
while (data1[1][k,1]!= ifs) {
  k <- k+1
}

m <- data1[2][k,1]


# Starting Point
x_0 <- data1[7][k,1]
y_0 <- data1[8][k,1]

# scale
lambda <- as.numeric(data1["lambda"][k:(k+m-1),1])

# rotation angles
theta <- as.numeric(data1["theta"][k:(k+m-1),1])

# rotation matrices 
R <- list()
for(i in 1:m){
  R[[i]] <- matrix(data=c(cos(theta[i]), -sin(theta[i]), sin(theta[i]), cos(theta[i])), nrow = 2, ncol = 2, byrow = TRUE)
}

# shifts 
V <- list()

for(j in 1:m){
  x <- as.numeric(data1["x_shift"][k:(k+m-1),1])
  y <- as.numeric(data1["y_shift"][k:(k+m-1),1])
  V[[j]] <- c(x[j], y[j])
}

# the ifs, FF(i, x) = f_i(x)
FF <- function(i, input_vector){
  output_vector <- c(lambda[i]*unlist(R[[i]])%*%input_vector + unlist(V[[i]]))
  return(output_vector)}

# Decisions for which function to use in each iteration 
# P(v_i) = 1/m for 1 = 1, ..., m
D <- sample(c(1:m),N,replace = TRUE)

X <- list() # A list to hold the points of the gasket

# Starting point
X[[1]] <- c(x_0,y_0)

# Apply the functions, in the order of D, to (x_0, y_0)
for(n in 2:N){
  X[[n]] <- FF(D[[n-1]], unlist(X[[n-1]]))
}

# Prepare for plotting
M<- c(x_0,y_0)
for(i in 2:N){
  M <- rbind(M,X[[i]])  
}

# Plot the points
plot(x=M[,1],y=M[,2],
     xlab ="",ylab="",pch='.',frame.plot=FALSE, axes = FALSE)

