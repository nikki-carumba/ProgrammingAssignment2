## Caching the Inverse of a Matrix
## To optimize the matrix inversion, the following functions cache 
## the result of an inversion of a matrix. These functions are referenced
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
  setInverse <- function() {
    m <<- solve(x) ## computes inverse of matrix
  }
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve inverses the matrix created from makeCachMatrix.
## First, it checks if inverse is already computed and the matrix 
## has not been changed, then, the cached inversed is retrieved.
## This function assumes that matrix is a square matrix, thus,
## square invertible

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
