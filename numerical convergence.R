# Emission (overall), E = N*e
E <- seq(0:99)

# Matrix for accumulated emissions over time as a function of E 
G <- matrix(rep(0,100*100),nrow = 100, ncol = 100, byrow = TRUE)

# Absorption rate (constant)
mu <- 0.2

# index (also max number of periods considered)
n <- 100

# Function which assigns every element of the previous matrix
# Always start with emission level of 0, and emit in period 2
# For any given E, the element in G corresponds to the accumulated emissions 
# up to that period. 

for(j in 1:n){
  for(i in 2:n){
    G[j,1] = 0 
    G[j,i] = (1-mu)*G[j,i-1]+E[j]
  }
} 

# give out the results for G
G

# plot G at t=100
plot(E,G[,100], title = 'Accumulated Emissions after 100 Periods as Function of E')

# plot the convergence of all G's over time
plot(1:100,G[1,1:100], ylim = c(0,500), type = 'l')
for(i in 2:100){par(new=TRUE);plot(1:100,G[i,1:100], add = TRUE, type = 'l', ylim = c(0,500))}

