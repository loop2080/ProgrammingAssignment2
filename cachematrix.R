## This file includes two functions;
## makeChacheMatrix:  This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve:        This function computes the inverse of the special "matrix" returned by 
##                    makeCacheMatrix above. If the inverse has already been calculated (and the 
##                    matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

####################################################################################################
## makeCacheMatrix: creates and returns an object that computes and caches the inverse matrix of 'x'

makeCacheMatrix <- function(x = matrix()) {
  
  # make sure cached value is NULL
  cache <- NULL
  
  # create the 'set' function, reset cache
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # create the 'get' function
  get <- function() x
  
  # create the 'setinverse' function
  setinverse <- function(inverse) cache <<- inverse
  
  # create the 'getinverse' function
  getinverse <- function() cache
    
  # return a list with the object's functions
  return(list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse))
  
}

######################################################################################
## cacheSolve: efficiently returns the inverse of 'x' using a simple caching technique

cacheSolve <- function(x, ...) {
        
  # check to see if there's a cached value
  cache <- x$getinverse()
  if (!is.null(cache)) {

    # there is a cached result, return the cached result
    message("getting cached data")
    return(cache)
  
  } else {
    
    # there is no cached result, compute, store the result in the cache and return the computed inverse
    message("calculating and caching data")
    data <- x$get()
    result <- solve(data, ...)
    x$setinverse(result)
    return(result)
  }
}
