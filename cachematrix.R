makeSpecialMatrix <- function(initialMatrix = matrix()) {
  # Create a special "matrix" object that can cache its inverse# Create a special "matrix" object that can cache its 
  mat <- initialMatrix
  cachedInverse <- NULL
  
  # Function to set the matrix
  setMatrix <- function(newMatrix) {
    mat <<- newMatrix
    cachedInverse <<- NULL  # Invalidate the cache
  }
  
  # Function to get the matrix
  getMatrix <- function() mat
  
  # Function to set the inverse
  setCachedInverse <- function(newInverse) cachedInverse <<- newInverse
  
  # Function to get the inverse# Function to 
  getCachedInverse <- function() cachedInverse
  
  # Return a list of functions
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCachedInverse = setCachedInverse,
       getCachedInverse = getCachedInverse)
}

getCachedInverse <- function(specialMatrix, ...) {
  # Return the cached inverse if available# Return the cached inverse if a
  inv <- specialMatrix$getCachedInverse()
  
  if (is.null(inv)) {
    # Calculate the inverse and cache it
    mat <- specialMatrix$getMatrix()
    inv <- solve(mat, ...)
    specialMatrix$setCachedInverse(inv)
  } else {
    message("Getting cached inverse")
  }
  
  # Return the cached inverse# Return the cached
  inv
}
