## Put comments here that give an overall description of what your
## functions do

## creates a custom matrix-"object" with the functions to cache an inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  # set the initial value for an "inverted" matrix to NULL
  inv <- NULL
  # function to set the matrix
  set <- function(y) {
    x <<- y
    # reset the "inverted" matrix
    inv <<- NULL
  }
  # function to return the "original" matrix
  get <- function() x
  # function to set the the cached "inverted" matrix
  setInverted <- function(inverted) inv <<- inverted
  # function to get the the cached "inverted" matrix
  getInverted <- function() inv
  # return our functions in the list
  list(set = set, get = get,
       setInverted = setInverted,
       getInverted = getInverted)
}


## calculates the inverted matrix of the custom matrix-"object"
cacheSolve <- function(x, ...) {
  inv <- x$getInverted()
  # check if a cached value is available
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # ...it's not
  mat <- x$get()
  # calculate the inverted matrix, propagating the input params (...) 
  inv <- solve(mat, ...)
  # update the cache
  x$setInverted(inv)
  # and return the result
  inv
}
