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




