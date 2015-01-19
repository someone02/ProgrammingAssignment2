## Introduction to R Programming - Assignment 2
## Pair of functions that cache the inverse of a matrix

## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## xinv member caches the inverse of x
  xinv <- NULL

  ## set function sets the matrix
  set <- function(y) {
    x    <<- y
    xinv <<- NULL
  }

  ## get function returns the matrix
  get <- function() x

  ## setinv function sets the inverse
  setinv <- function(inv) xinv <<- inv

  ## getinv function gets the inverse
  getinv <- function() xinv

  ## return the final object
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the "matrix" object returned by
## makeCacheMatrix. If the inverse has already been computed,
## (and the matrix has not changed), then the function will
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Try to retrieve inverse from the cache
  xinv <- x$getinv()

  ## Check if it exists
  if (!is.null(xinv)) {
    ## return cached inverse if so
    message("getting cached data")
    return(xinv)
  }

  ## If not, compute the inverse
  data <- x$get()
  xinv <- solve(data)
  ## and set it
  x$setinv(xinv)

  ## Return a matrix that is the inverse of 'x'
  xinv
}
