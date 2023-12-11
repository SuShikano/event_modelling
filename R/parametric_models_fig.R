# Exponential models

expo.h.t <- function(t){
  exp( beta0 + beta1*x)*t/t
}

expo.S.t <- function(t){
   exp(- expo.h.t(t) * t)
}

expo.f.t <- function(t){
  expo.h.t(t) * exp(- expo.h.t(t) * t)
} 

t <- 2
beta0 <- -0.5
beta1 <- 0.2
x <- 5

pdf(file="figures/Exponentialmodel_S_f_h.pdf",onefile = F,
    height = 3,width=8,pointsize=13)

par(mfrow=c(1,3))
add.vec <- c(F,rep(T,4))
for (x in c(1,3,5)){
  curve(expo.S.t,0,10,xlab="t",ylab="S(t)",
        main="S(t)",ylim=c(0,1),add=add.vec[x],
        lwd=x/2)
}
legend("topright",lty=1,lwd=c(1,3,5)/2,legend=paste0("x=",c(1,3,5)),bty="n")
for (x in c(1,3,5)){
  curve(expo.f.t,0,10,xlab="t",ylab="f(t)",
        main="f(t)",ylim=c(0,1.5),add=add.vec[x],
        lwd=x/2)
}
for (x in c(1,3,5)){
curve(expo.h.t,0,10,xlab="t",ylab="h(t)",
      main="h(t)",ylim=c(0,2),add=add.vec[x],
      lwd=x/2)
}
dev.off()



# Log normal models


t <- 2
beta0 <- 0.7
beta1 <- 0.3
x <- 1
sigma <- 0.5

lognorm.S.t <- function(t){
  1- pnorm(log(t),mean= beta0 + beta1*x,sd=sigma)
}


lognorm.f.t <- function(t){
  dnorm(log(t), mean=beta0 + beta1*x,sd= sigma) / t
}

lognorm.h.t <- function(t){
  lognorm.f.t(t)  / lognorm.S.t(t)
}


pdf(file="figures/Lognormalmodel%01d.pdf",onefile = F,
    height = 3,width=8,pointsize=13)
for (sigma in c(2,1,0.5)){
par(mfrow=c(1,3))
curve(lognorm.S.t,0,10,xlab="t",ylab="S(t)")
curve(lognorm.f.t,0,10,xlab="t",ylab="f(t)",
      main=paste0("sigma=",sigma,"; p=",1/sigma))
curve(lognorm.h.t,0,10,xlab="t",ylab="h(t)")
}
dev.off()


pdf(file="figures/Lognormalmodel_ph.pdf",onefile = F,
    height = 3,width=8,pointsize=13)
sigma = 0.5
par(mfrow=c(1,3))
for (x in c(0,1,2)){
  curve(lognorm.h.t,0,10,xlab="t",ylab="h(t)",ylim=c(0,1),
        main=paste0("x=",x))
}
dev.off()




