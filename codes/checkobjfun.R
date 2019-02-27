
R=matrix(c(100,100,100,75,75,75,25,25,25),nrow = 3,ncol = 3,byrow = TRUE)
M=matrix(c(160,80,40,160,80,40,160,80,40),nrow = 3,ncol = 3,byrow = TRUE)
p=matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
P=matrix(rep(colSums(p),3),nrow = 3,ncol = 3,byrow = TRUE)

## varying sigma (we vary Mh and Lh because these are the values that oscillate)
lambda=0.7
sigma=c(0.99,0.01,0.01,0.5,0,0,0.5,0.9,1)
x=seq(0.4,0.8,0.01)
n=length(x)
y=rep(0,n)
for(i in seq(1,n)){
  sigma[4]=x[i]
  sigma[7]=x[i]
  y[i]=qre_distance(sigma,lambda,P,R,M)
}
plot(x,y)


## varying lambda
lambda=0.75
sigma=c(0.99,0.0,0.0,0.5,0.01,0.01,0.5,0.9,1)
x=seq(0.7,1,0.05)
n=length(x)
y=rep(0,n)
for(i in seq(1,n)){
  lambda=x[i]
  y[i]=qre_distance(sigma,lambda,P,R,M)
}
plot(x,y)