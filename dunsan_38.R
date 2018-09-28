### Remove all objects
rm(list=ls(all=TRUE))

# data read
mydata.original <- read.table("D:/data/dunsan_trip_38.csv",sep = ",", header=TRUE)
mydata.1 = mydata.original

# select only value (time*pair)
X.data = mydata.1[,3:ncol(mydata.1)]
X.data = as.matrix(X.data)

#mean-deviation form (by column)
mean.X.data = apply(X.data, 2, mean)
X.data.cen <- data.frame()
for (i in 1:length(X.data[,1])) {
  X.data.cen = rbind(X.data.cen, X.data[i,] - mean.X.data)
}

X.data.cen = as.matrix(X.data.cen)

# make Covariance matrix
cov.X.data = (t(X.data.cen) %*% X.data.cen) / (ncol(X.data)-1)

# Eigenvalue decomposition
time.n1.1 = Sys.time()
my.eigen = eigen(cov.X.data)
time.n1.2 = Sys.time()
time.n1.2 - time.n1.1

# projection------------------------- 

# each root lamda(eigenvalue)
my.ev = my.eigen$values
my.ev.posi.label = my.ev > 0
my.ev.posi = my.ev[my.ev.posi.label]
my.ev.posi.root = my.ev.posi^ 0.5

# each eigenvector
my.evt = my.eigen$vectors

# calulate vector u
my.u <- data.frame()
for (i in 1:ncol(X.data)) {
  my.u.a <- as.vector(X.data %*% my.evt[,i]) / my.ev.posi.root[i]
  my.u <- rbind(my.u, t(my.u.a))  
}

# projection 
project <- list()
for (i in 1:ncol(X.data)) {
  project.a <- my.ev.posi.root[i] * t(my.u[i,])
  project[[length(project)+1]] <- list(project.a)
}









#scree plot
plot(my.eigen$values[2:ncol(X.data)], type='o')
my.eigen$values[2:ncol(X.data)]

#cummulative variance
cdf.eigen = ecdf(as.numeric(my.eigen$values))
plot(cdf.eigen)

## Approximation--------------------- 

# approximation for each eigenvector
approx <- list()
for (i in 1:ncol(X.data)) {
  approx.a <- my.ev.posi.root[i] * t(my.u[i,]) %*% my.evt[,i]
  approx[[length(approx)+1]] <- list(approx.a)
}

#reconstruction (sum of approximatation of each vector)
X_hat.1com = as.data.frame(approx[[1]]) 
X_hat.2com = X_hat.1com + as.data.frame(approx[[2]]) 
X_hat.4com = X_hat.2com + as.data.frame(approx[[3]]) + as.data.frame(approx[[4]]) 
X_hat.7com = X_hat.4com + as.data.frame(approx[[5]]) + as.data.frame(approx[[6]]) + as.data.frame(approx[[7]]) 
X_hat.all = X.data %*% my.eigen$vectors %*% t(my.eigen$vectors)


# Value comparison of some point
X.data[35:37,1]
X_hat.all[35:37,1]
X_hat.7com[35:37,1]
X_hat.4com[35:37,1]
X_hat.2com[35:37,1]
X_hat.1com[35:37,1]

X.data[35:37,2]
X_hat.all[35:37,2]
X_hat.7com[35:37,2]
X_hat.4com[35:37,2]
X_hat.2com[35:37,2]
X_hat.1com[35:37,2]


# Plot comparison (all day)
library(rminer)
par(mfrow = c(3,1))

# Xhat.all 
mgraph(X.data[1:120,1],X_hat.all[1:120,1],graph="REG",Grid=10,col=c("black","blue"))
mgraph(X.data[1:120,2],X_hat.all[1:120,2],graph="REG",Grid=10,col=c("black","blue"))
mgraph(X.data[1:120,5],X_hat.all[1:120,5],graph="REG",Grid=10,col=c("black","blue"))

#X.hat using 7components
mgraph(X.data[1:120,1],X_hat.7com[1:120,1],graph="REG",Grid=10,col=c("black","blue"))
mgraph(X.data[1:120,6],X_hat.7com[1:120,6],graph="REG",Grid=10,col=c("black","blue"))
mgraph(X.data[1:120,12],X_hat.7com[1:120,12],graph="REG",Grid=10,col=c("black","blue"))

cor(X.data[1:120,1],X_hat.7com[1:120,1])
cor(X.data[1:120,6],X_hat.7com[1:120,6])
cor(X.data[1:120,12],X_hat.7com[1:120,12])

#X.hat using 4components
mgraph(X.data[1:120,1],X_hat.4com[1:120,1],graph="REG",Grid=10,col=c("black","blue"))
mgraph(X.data[1:120,6],X_hat.4com[1:120,6],graph="REG",Grid=10,col=c("black","blue"))
mgraph(X.data[1:120,12],X_hat.4com[1:120,12],graph="REG",Grid=10,col=c("black","blue"))

cor(X.data[1:120,1],X_hat.4com[1:120,1])
cor(X.data[1:120,6],X_hat.4com[1:120,6])
cor(X.data[1:120,12],X_hat.4com[1:120,12])

#X.hat using 2components
mgraph(X.data[1:120,1],X_hat.2com[1:120,1],graph="REG",Grid=10,col=c("black","blue"))
mgraph(X.data[1:120,6],X_hat.2com[1:120,6],graph="REG",Grid=10,col=c("black","blue"))
mgraph(X.data[1:120,12],X_hat.2com[1:120,12],graph="REG",Grid=10,col=c("black","blue"))

cor(X.data[1:120,1],X_hat.2com[1:120,1])
cor(X.data[1:120,6],X_hat.2com[1:120,6])
cor(X.data[1:120,12],X_hat.2com[1:120,12])

#X.hat using 1components
mgraph(X.data[1:120,1],X_hat.1com[1:120,1],graph="REG",Grid=10,col=c("black","blue"))
mgraph(X.data[1:120,6],X_hat.1com[1:120,6],graph="REG",Grid=10,col=c("black","blue"))
mgraph(X.data[1:120,12],X_hat.1com[1:120,12],graph="REG",Grid=10,col=c("black","blue"))

cor(X.data[1:120,1],X_hat.1com[1:120,1])
cor(X.data[1:120,6],X_hat.1com[1:120,6])
cor(X.data[1:120,12],X_hat.1com[1:120,12])


# three trend, 1st od pair
plot(as.data.frame(approx[[1]])[,1],type="l")
plot(as.data.frame(approx[[2]])[,1],type="l")
plot(as.data.frame(approx[[3]])[,1],type="l")

plot(as.data.frame(approx[[4]])[,1],type="l")
plot(as.data.frame(approx[[5]])[,1],type="l")
plot(as.data.frame(approx[[6]])[,1],type="l")

plot(as.data.frame(approx[[7]])[,1],type="l")
plot(as.data.frame(approx[[8]])[,1],type="l")
plot(as.data.frame(approx[[9]])[,1],type="l")

plot(as.data.frame(approx[[10]])[,1],type="l")
plot(as.data.frame(approx[[11]])[,1],type="l")
plot(as.data.frame(approx[[12]])[,1],type="l")

plot(as.data.frame(approx[[13]])[,1],type="l")
plot(as.data.frame(approx[[14]])[,1],type="l")
plot(as.data.frame(approx[[15]])[,1],type="l")

plot(as.data.frame(approx[[16]])[,1],type="l")
plot(as.data.frame(approx[[17]])[,1],type="l")
plot(as.data.frame(approx[[18]])[,1],type="l")

plot(as.data.frame(approx[[19]])[,1],type="l")
plot(as.data.frame(approx[[20]])[,1],type="l")
plot(as.data.frame(approx[[21]])[,1],type="l")

plot(as.data.frame(approx[[22]])[,1],type="l")
plot(as.data.frame(approx[[23]])[,1],type="l")

# three trend, 2nd od pair
par(mfrow = c(3,1))
plot(as.data.frame(approx[[1]])[,2],type="l")
plot(as.data.frame(approx[[3]])[,2],type="l")
plot(as.data.frame(approx[[16]])[,2],type="l")

# contribution of three trend for each od pair
con.apox.st = as.data.frame(approx[[1]])
con.apox.st_de = X_hat.4com - as.data.frame(approx[[1]])
con.apox.stocha = X.data - con.apox.st - con.apox.st_de

con.apox.st.sum = apply(con.apox.st,2,sum)
con.apox.st_de.sum = apply(con.apox.st_de,2,sum)
con.apox.stocha.sum = apply(con.apox.stocha,2,sum)
X.data.colsum = apply(X.data,2,sum)

con.apox = cbind(con.apox.st.sum,con.apox.st_de.sum,con.apox.stocha.sum,X.data.colsum)
for (i in 1:3){
  con.apox[,i] = con.apox[,i] / con.apox[,4] 
}

par(mfrow = c(1,1))
matplot(con.apox,type = c("b"),ylim=c(-0.2,1.2))

## loading matrix analysis--------------------- 
# Principal complnent analysis
pca.X.data = princomp(cov.X.data)
pca.loadings = pca.X.data$loadings[,1:ncol(cov.X.data)]

# find the number of specific PC larger than 1/root(p)
pca.loadings.spe = abs(pca.loadings) - (1/(ncol(X.data)^0.5))
pca.loadings.spe.posi = pca.loadings.spe > 0
pca.loadings.spe.posi.sum = apply(pca.loadings.spe.posi,1,sum)

# hist of number of component for each OD pair
par(mfrow = c(1,1))
hist(pca.loadings.spe.posi.sum)

# spedific valued PC and OD pair  
library(fields)
colorTable<- designer.colors(11, c( "blue","white", "red") )
brks<- c(seq( 1, 10,,6), seq( 17, 25,,6)) 
image.plot(pca.loadings.spe.posi[1:23,1:22],nlevel = 2, breaks=brks, col=colorTable)





