## The functions makeCacheMatrix and cacheSolve enables you to create a
## special matrix object that can cache its inverse.  Calculating the
## inverse of a matrix can be a costly computation and unless the matrix
## has changed, using a cached inverse is much more efficient.
##
## Assumptions:
## 1) Matrix supplied to makeCacheMatrix is always invertible

## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Set the stored matrix inverse to NULL upon initialization.
  matrixinverse <- NULL

  # Update the stored matrix to a new matrix
  set <- function(newmatrix) {
    
    # Update x to the new matrix
    x <<- newmatrix
    
    # Set the stored matrix inverse to NULL since the matrix is new
    # and the inverse has not been calculated yet.
    matrixinverse <<- NULL
  }
  
  # get returns the stored matrix
  get <- function() x
  
  # setinverse stores the inverse of a matrix
  setinverse <- function(inverse) matrixinverse <<- inverse
  
  # getinverse returns the stored matrix inverse
  getinverse <- function() matrixinverse
  
  # Returns a list containing the four functions for the special matrix object
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve calculates and caches the inverse of a special matrix object
## created by the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
  # Get the current inverse from the special matrix x
  m <- x$getinverse()
  
  # If the curent inverse of x is not NULL then use cached data
  if (!is.null(m)) {
    # Display a message notifying the user that cached data is used
    message("getting cached data")
    
    # Return the current cached inverse from special matrix x
    return(m)
  }
  # Get the stored matrix
  data <- x$get()
  
  # Calculate the matrix inverse
  m <- solve(data, ...)
  
  # Update the stored inverse
  x$setinverse(m)

  # Return the matrix inverse
  m
}
