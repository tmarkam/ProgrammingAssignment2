## cachematrix.R
## Coursera R Programming Assignment - week 3 - Lexical scoping
## functions to support cached operations on matrices

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## return a list of functions, to:
  ##  set the value of the matrix
  ##  get the value of the matrix
  ##  set the value of the inverse
  ##  get the value of the inverse
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL  ## when the matrix changes, void the cached inverse
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## compute the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## If the inverse is cached, return the cache copy
  ## else recalculate the inverse, cache it and return it
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
