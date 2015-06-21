# Week 3 Assignment 2 from Emma Y

# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse of a matrix rather than compute itrepeatedly
# (there are also alternatives to matrix inversion that we will not discuss here).  
# My assignment is to write a pair of functions that cache the inverse of a matrix.

# 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## How to use this? Sample:
## > x = rbind(c(1,3), c(4,6))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    4    6
## > cacheSolve(m)
##            [,1]       [,2]
## [1,] -1.0000000  0.5000000
## [2,]  0.6666667 -0.1666667
## > cacheSolve(m)
## getting cached data
##            [,1]       [,2]
## [1,] -1.0000000  0.5000000
## [2,]  0.6666667 -0.1666667
## > 