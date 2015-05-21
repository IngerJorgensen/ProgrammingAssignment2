## Put comments here that give an overall description of what your
## functions do

## The first function makeCacheMatrix takes a square matrix as input
## and gives a list object of 4 functions as output. Those 4 
## functions can set or get the data (the matrix), set or get the
## inverse of that matrix
## The second function cacheSolve takes as input the list from 
## makeCacheMatrix. The output is the inverse of the matrix used
## as input to makeCacheMatrix, either retrieved from cache or
## derived if not available in the cache.

## Write a short comment describing this function
## The function makeCacheMatrix takes a square matrix as input
## It then returns a list of 4 functions to set or get the data
## and to set or get the inverse of the matrix (setinv and getinv)
## To use this, first source this file
##   source("cachematrix.R"
## Then let us have some data, say 
##   x <- matrix(rnorm(9),nrow=3,ncol=3)
## Then invoke the function and store the result in say object a
##   a <- makeCacheMatrix(x)
## Strictly speaking the function ought to check that the matrix
## is at least square, but since the assignemnt says to assume
## the matrix is invertible and only square matrices are, I have 
## not added that check.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function operates on the list of functions from above
## using the content of the matrix in the parent environment
## I think it is a bit misleading to say it returns the invernse
## of the parameter in the argument list. It returns the inverse
## of the matrix that was used as argument for makeCacheMatrix
## To use the function, assume you have done what was described 
## in the comments above makeCacheMatrix, so the functions are
## contained in the object a. Then to get the inverse
## of the matrix x
##    cacheSolve(a)
## First time it will calculate it. Second time (a was not changed)
## it will retrieve the cached solution.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
