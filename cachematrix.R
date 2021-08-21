## These 2 functions are use together to make a cache matrix
## and inverse of cached matrix for future use

## makeCacheMatrix function takes argument x as a matrix to be cache
## by assign makeCacheMatrix to an object
## for example mymatrix <- makeCacheMatrix(<Matrix of your choice>)
## This will cache the matrix inside an object mymatrix for future reference

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
    ## set function takes new matrix into an object and reset inverse to NULL
    ## can be used to set new matrix of your choice to be solved
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function takes argument x as an object created from makeCacheMatrix
## by calling cacheSolve on previously defined mymatix
## cacheSolve see if any cached inverse matrix is stored and return them
## if no inverse matrix is stored then, it will calculate an inverse matrix
## and return them

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}