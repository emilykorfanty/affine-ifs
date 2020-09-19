#=====================
# Sierpinski Carpet
#=====================
#
#
#

# = Pick a number of iterations =

N <- 1

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

F_4 <- function(X){
  Y <- list()
  Y[[1]]<-f_4(X[[1]])
  Y[[2]]<-f_4(X[[2]])
  Y
  
}

F_5 <- function(X){
  Y <- list()
  Y[[1]]<-f_5(X[[1]])
  Y[[2]]<-f_5(X[[2]])
  Y
  
}

F_6 <- function(X){
  Y <- list()
  Y[[1]]<-f_6(X[[1]])
  Y[[2]]<-f_6(X[[2]])
  Y
  
}

F_7 <- function(X){
  Y <- list()
  Y[[1]]<-f_7(X[[1]])
  Y[[2]]<-f_7(X[[2]])
  Y
  
}

F_8 <- function(X){
  Y <- list()
  Y[[1]]<-f_8(X[[1]])
  Y[[2]]<-f_8(X[[2]])
  Y
  
}

# = = = = = = = = = = = = 

# = Define the starting set =

ab<-list()
ab[[1]]<-c(0, 0)
ab[[2]]<-c(1, 0)

bc<-list()
bc[[1]]<-c(1, 0)
bc[[2]]<-c(1, 1)

cd<-list()
cd[[1]]<-c(1, 1)
cd[[2]]<-c(0, 1)

de<-list()
de[[1]]<-c(0,1)
de[[2]]<-c(0, 0)

S<-list()
S[[1]] <- ab
S[[2]] <- bc
S[[3]] <- cd
S[[4]] <- de

# Plot the starting set
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
  geom_line() + xlab("") + ylab("") + theme(aspect.ratio=1) + 
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


# = = = = = = = = = = = = = = = = = = = = = = = = =



# = = = = = = = = = = = = 

# = Iterate the functions =

FF <- list()

for(i in 1:N){
  
  for(j in 1:length(S)){
    
    FF[[j]] <- list(F_1(S[[j]]),
                    F_2(S[[j]]), 
                    F_3(S[[j]]),
                    F_4(S[[j]]),
                    F_5(S[[j]]),
                    F_6(S[[j]]),
                    F_7(S[[j]]),
                    F_8(S[[j]]))
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
  geom_line() + xlab("") + ylab("") + theme(aspect.ratio=1) +
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




