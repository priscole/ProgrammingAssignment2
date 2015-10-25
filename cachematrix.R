## The functions create and consume a series of sub-functions, which are 
## able to cache a matrix, and solve and cache its inverse. If a new matrix
## is supplied, the cache is reset.

## The function makeCacheMatrix creates a list of four sub-functions,
## ready to be consumed. The default is to create an empty matrix container
## which is cached via set. The set function also makes an empty container 
## for the inverse of the matrix, and it is cached as well. If a new matrix
## is supplied to the the set function, the cachedMatrixInverse
## is reset to NULL. If the setInverse function is called, it calculates the
## inverse of the matrix and stores it in the cache container. The forth
## function getInverse can be called to retrieve the values of the
## cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  cachedMatrixInverse <- NULL
  set <- function(y){
    x <<- y
    cachedMatrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedMatrixInverse <<- inverse
  getInverse <- function() cachedMatrixInverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The cacheSolve function consumes 1 to 3 of the 4 functions stored in the 
## list above. First it checks to see if there's anything cached in the
## cachedMatrixInverse container. If an inverse is cached, it's returned
## and the function stops there. If there's no cached inverse, then the 2nd
## piece of the function executes, which grabs the data, solves the inverse
## of the matrix, stores it in the cache, and then returns it.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
