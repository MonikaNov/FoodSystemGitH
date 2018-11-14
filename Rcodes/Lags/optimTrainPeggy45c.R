library(lpSolve)
# Set up problem: maximize
# x1 + 9 x2 + x3 subject to
# x1 + 2 x2 + 3 x3 <= 9
# 3 x1 + 2 x2 + 2 x3 <= 15
#
f.obj <- c(1, 9, 1)
f.con <- matrix (c(1, 2, 3, 3, 2, 2), nrow=2, byrow=TRUE)
f.dir <- c("<=", "<=")
f.rhs <- c(9, 15)
#
# Now run.
#
lp ("max", f.obj, f.con, f.dir, f.rhs)
## Not run: Success: the objective function

#000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#
f.obj <- c(1, -2)
f.con <- matrix (c(1, 0, 1, 0, 0, 1,0,1), nrow=4, byrow=TRUE)
f.dir <- c("<=", ">=","<=",">=")
f.rhs <- c(4, 2,-1,-6)
#
# Now run.
#
lp ("max", f.obj, f.con, f.dir, f.rhs)
lp ("max", f.obj, f.con, f.dir, f.rhs)$solution
## Not run: Success: the objective function


#000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#
library(optimx)
optimx(par, fn, gr=NULL, hess=NULL, lower=-Inf, upper=Inf,
       method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
       control=list(),
       ...)

test1<-optimx(c(0,0), fn=function(x)  x[1]-2*x[2] , gr=NULL, hess=NULL, lower=c(-1,-6), upper=c(4,1),
       method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
       control=list())

test1


test2<-optimx(c(0,0), fn=function(x)  -(x[1]-2*x[2]) , gr=NULL, hess=NULL, lower=c(-1,-6), upper=c(4,1),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list())

test2

#000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# SEPERATELY EAST AND WEST HAS TO BE I GUESS

koefs<-as.numeric(summary(Peggy45)$coeff[,1])
objF<-function(x,k) x*k 

aa=function(x,k=koefs) -sum(x*k)
aa=function(x,k=koefs) -(k[1]+sum(x*k[-1]))


aa(c(rep(0,8)))

#west:
test3<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,3,5,7,9)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list())
test3

test4<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,3,5,7,9)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list(maximize =TRUE))
test4    
              
test5<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,3,5)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list())
test5

test6<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,3,5)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list(maximize =TRUE))
test6    

              

#east:

testE3<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,2,4,6,8)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list())
testE3
test3

testE4<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,2,4,6,8)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list(maximize =TRUE))
testE4    
test4

testE5<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,2,4)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list())
testE5
test5

testE6<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,2,4)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list(maximize =TRUE))
testE6    
testE4
test6



              
              
              

test4
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# SEPERATELY EAST AND WEST HAS TO BE I GUESS

# now how would it be without the extra variables:

test5<-optimx(c(rep(0,4)), function(x,k=koefs[1:5]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list())

test5 # vs
test3


test6<-optimx(c(rep(0,4)), function(x,k=koefs[1:5]) +(k[1]+sum(x*k[-1])), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list(maximize=TRUE))

test6 #vs 
test4


#yield:  -0.05879081 - (test3)

-0.05879081-  -0.862519   
sd(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)

yld<-(-0.05879081*sd(dataUsTS$Yield,na.rm=TRUE))+mean(dataUsTS$Yield,na.rm=TRUE)
yld

yld2<-(-0.3119225 *sd(dataUsTS$Yield,na.rm=TRUE))+mean(dataUsTS$Yield,na.rm=TRUE)
yld2


DataP45<-dataScTS[rownames(dataScTS)%in% rownames(  model.frame(Peggy45)),]
DataUsP45<-dataUsTS[rownames(dataUsTS)%in% rownames(  model.frame(Peggy45)),]

summary(DataUsP45$Yield)

mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
sd(dataUsTS$Yield,na.rm=TRUE)

