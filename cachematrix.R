## This function creates a special matrix that can be cached and used in other calculations
## functions do

## Sets the value of the matrix, gets the value of the matrix and set/get the inverse

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
  x<<-y
  m<<-NULL
 } 
  get<-function()x
  setinv<- function(solve)m<<-solve
  getinv<-function()m
  list(set=set,get=get,
       setinv=setinv,getinv=getinv)

}


## Create the inverse of a matrix, first checking if a cached value exists to reduce computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-mean(data,...)
  x$setinv(m)
  m
}
