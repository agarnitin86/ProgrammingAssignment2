## Put comments here that give an overall description of what your
## functions do

# There are two functions, 
# makeCacheMatrix : creates a special caching matrix
# cacheSolve : Computes inverse or fetches the inverse of matrix(if inverse is in cache)


## Write a short comment describing this function

# This function creates the special caching matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inve) inverse <<- inve
  
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

# This function first checks whether the inverse of matrix has already been computed.
# If it has already been compted, it returns the inverse from the cache.
# If not then it computes the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
