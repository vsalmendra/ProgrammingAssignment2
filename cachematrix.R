## Code to implement a matrix that can cache the value of its inverse,
## in order to avoid repeated costly inverse computations

## Creates a kind of matrix that can cache the value of its inverse
makeCacheMatrix <- function(x = matrix()) {

  # Variable to hold the inverse
  inverse <- NULL
  
  set <- function(y) {
    # Updates the value of the matrix
    x <<- y
    
    # Clears the cache, since matrix was modified
    inverse <<- NULL
  }
  
  # Function to return the matrix
  get <- function() x
  
  # Function to set the value of the inverse
  setinverse <- function(i) inverse <<- i
  
  # Function to cache the value of the inverse
  getinverse <- function() inverse
  
  # Returns the list of created functions (notice that they all will
  # refer to the same environment)
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}


## Function to calculated the inverse of a CacheMatrix

cacheSolve <- function(x, ...) {
        
  # Check whether there is a cached inverse. If to, returns it
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # No cached inverse found. Calculates the inverse
  m <- x$get()
  i <- solve(m,...)
  
  # Caches the inverse value
  x$setinverse(i)
  
  # Returns the inverse
  i
}
