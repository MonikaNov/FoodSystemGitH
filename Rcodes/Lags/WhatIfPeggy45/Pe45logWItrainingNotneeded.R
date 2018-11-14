aa<-function(x,k=koefs[c(1,3,5,7,9)]) exp(koefs[1]+sum(x*k[-1]))

aa(c(0,0,0,0))

bb<-function(x,k=koefs[c(1,3,5,7,9)]) (exp(koefs[1])*exp(x[1]*koefs[2])*exp(x[2]*koefs[3])*exp(x[3]*koefs[4])*exp(x[4]*koefs[6]))

bb(c(0,0,0,0))


logAllminW<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,3,5,7,9)]) exp(koefs[1]+sum(x*k[-1])), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
                   method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
                   control=list()); logAllminW


logAllminW<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,3,5,7,9)]) (exp(koefs[1])*exp(x[1]*koefs[3])*exp(x[2]*koefs[5])*exp(x[3]*koefs[7])*exp(x[4]*koefs[9])), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
                   method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
                   control=list()); logAllminW