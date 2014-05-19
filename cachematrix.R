##Programming Assignment 2 : Caching the inverse of a matrix
## There are two functions, 
## 1. makeCacheMatrix : creates a special caching matrix
## 2. cacheSolve : Computes inverse or fetches the inverse of matrix(if inverse is in cache)

# This function creates the special caching matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  ## Function to initialize the matrix and its inverse
  ## Initially inverse is set to NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## Function to the return the original Matrix
  get <- function() x
  
  ## Function to set inverse for the matrix.
  setinverse <- function(inve) inverse <<- inve
  
  ## Function to return inverse of the matrix.
  getinverse <- function() inverse
  
  ## List of functions which can be called from cacheSolve function.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

# This function first checks whether the inverse of matrix has already been computed.
# If it has already been compted, it returns the inverse from the cache.
# If not then it computes the inverse and returns it.

cacheSolve <- function(x, ...) {
        
  ## Get the computed inverse of matrix x
  inv <- x$getinverse()
  
  ## If inverse has been computed, return the inverse.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If inverse was not already computed,
  ## Get the original matrix
  data <- x$get()
  
  ## Compute the inverse
  inv <- solve(data, ...)
  
  ## Set the inverse of x
  x$setinverse(inv)
  
  ## Return the inverse
  inv
}
