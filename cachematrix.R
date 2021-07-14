## Put comments here that give an overall description of what your
## functions do

## the makecachematrix function lets you create a matrix and then subject it to the 
## get and get inverse commands. If doing getinverse gives you a null response
## then use cachesolve in order to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ... ) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("retrieving cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}