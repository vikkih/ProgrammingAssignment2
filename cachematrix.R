## This script computes the inverse of the input matrix. If
## the inverse has already been calculated (and the matrix
## has not changed), the inverse is retrieved from the cache
## in order to save some computation time.

## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  # Create a NULL object for the inverse
  inverse <- NULL
  
  # Set the matrix to y
  set <- function(y) {
    
	      # Substitute matrix x with y globally
          x <<- y
		  # Set inverse to null object globally
          inverse <<- NULL
          
  }
  
  # Return value of matrix x
  get <- function() x
  
  # Set the inverse matrix to value of "inverse_matrix" globally
  setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
  # Return value of inverse
  getinverse <- function() inverse
  
  # List of functions in makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes and returns the inverse of the input
## matrix. If the inverse has already been calculated, a message
## is given and the cached inverse is returned. Otherwise the
## inverse is computed and returned.
cacheSolve <- function(x, ...) {
        
   ## Return a matrix that is the inverse of 'x'
   
   # Get inverse of input
   inverse <- x$getinverse()
   
   # If inverse is not a NULL object
   if(!is.null(inverse)) {
     
	      # Print message
          message("getting cached data")
          
		  # Return inverse (and exit function)
          return(inverse)
     
   }
   
   # Otherwise: assign input matrix to variable m
   m <- x$get()
   
   # Get inverse of the matrix
   inverse <- solve(m, ...)
   
   # Save inverse to cache
   x$setinverse(inverse)
   
   # Return inverse (and exit function)
   inverse


}
