## This file contains two functions: makeCacheMatrix and cacheSolve used 
## to create a special matrix object that can store (cache) its own inverse

## makeCacheMatrix generates a "matrix" object that 
## is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes and caches the inverse of a matrix object 
## created using makeCacheMatrix. If the inverse is already cached
## and the matrix hasn't changed, the function retrieves the cached
## inverse instead of computing it again.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}