## A set of functions to cache and retrieve the solution
## to a matrix

#Created by Chris Rupley for R Programming Coursera
#2014-08-23

## Creates a list of functions that either store a value in cache
## or retrieve it from cache

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solved) mat <<- solved
  getinverse <- function() mat
  list(set=set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Checks if solution is present in cache, if so it retrieves it,
## if not, it calculates the solution and caches it

cacheSolve <- function(x, ...) {
  mat <- x$getinverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}

#test
# 
# x <- matrix(1:4,2,2)
# mcm <- makeCacheMatrix(x)
# 
# #should return only matrix inverse
# cacheSolve(mcm)
#
# #should return "getting cached data" before inverse
# cacheSolve(mcm)
