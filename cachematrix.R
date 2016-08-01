## cachematrix.R
## Coursera R Programming Assignment - week 2
## functions to support cached operations on matrices

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## return a list, consisting of:
  ##   [1]=the matrix; 
  ##   [2]=cached copy of the matrix; 
  ##   [3]=cached inverse of the original (initialized to NA's)
  list(matx=x, cach=x, caci=matrix(data = NA, nrow = nrow(x), ncol = ncol(x)))
}


## compute the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ##   then the cachesolve should retrieve the inverse from the cache.
  
  ## if (a) the cached inverse matrix is missing, 
  ## or (b) the matrix has changed from the cached copy
  ## then re-cache the matrix and recalculate the inverse 
  if (all(is.na(x$caci)) | !identical(x$matx, x$cach)) {
    x$cach = x$matx
    x$caci = solve(x$cach)
  }
  ## return the cached inverse
  x$caci
}
