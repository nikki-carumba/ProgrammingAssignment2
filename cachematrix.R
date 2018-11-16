## Caching the Inverse of a Matrix
## To optimize the matrix inversion, the following functions caches 
## the result of inversion of the matrix. These functions are referenced
## from makeVector and cachemean(https://github.com/rdpeng/ProgrammingAssignment2) 
## that are used to create a special object that stores a numeric vector 
## and caches its mean.

## makeCacheMatrix creates a special matrix object that caches its inverse.
## It contains functions to set the value of the matrix, get the value of 
## the matrix, set the value of the inversed matrix, and get the value of 
## the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) {
    m <<- inverse 
  }
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve inverses the matrix created from makeCachMatrix.
## First, it checks if it already has inverse, and the matrix 
## has not been changed, then the cached inversed is retrieved.
## This function assumes that matrix is a square matrix, thus,
## square invertible

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) ## computes inverse of matrix
  x$setInverse(m)
  m
}
