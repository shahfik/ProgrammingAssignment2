## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Two functions are created, makeCacheMatrix and cacheSolve, to enable caching
## of matrices.
##
## Example:
## > a <- matrix(c(4,2,7,6), nrow=2,ncol=2)
## > b <- makeCacheMatrix(a)
## > cacheSolve(b)
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > cacheSolve(b)
## getting cached data
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated (and the
## matrix has not changed), then `cacheSolve` should retrieve the inverse from the
## cache.
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}