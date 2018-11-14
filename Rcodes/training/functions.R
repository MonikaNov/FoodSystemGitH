mylist=as.list(Phase23[1:10,5:8])

Train1 <- function(.) {2*mean(.)}
sapply(mylist,Train1)


Train2 <- function(x) {2*mean(x)}
sapply(mylist,Train2)



2*mean(mylist[[2]])

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
a <- list(b=1)
class(a) <- "myclass"
myfunction <- function(x,...) UseMethod("myfunction")
myfunction.myclass <- function(x,...) x$b+1
myfunction(a)

myfunction(4)