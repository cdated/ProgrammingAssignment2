## The following functions allow for a special matrix to be created that
## stores and retrieves its own inverse.  If a matrix has already had
## its inverse calculated it will automatically be recalled from cache.

## Create special matrix object with methods to set/get matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse as NULL
  m <- NULL
  
  ## Set the matrix and reset inverse matrix to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Recall the matrix
  get <- function() x
  
  ## Store the computed inverse matrix for later
  setInverse <- function(inverse) m <<- inverse
  
  ## Return the inverse matrix, NULL by default
  getInverse <- function() m
  
  ## Return the available methods for the special matrix object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Take the special matrix and output the inverse if it is already cached
## If it hasn't cached, solve the inverse and cache for later and return the inverse.
cacheSolve <- function(x, ...) {

  ## Return the current inverse value if it is available, not NULL.
  ## If the value is NULL if still needs to be calculated and stored
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix
  data <- x$get()
  
  # Calculate the inverse and store as 'm'
  m <- solve(data, ...)
  
  # Set the inverse for future recall
  x$setInverse(m)

  ## Return a matrix that is the inverse of 'x'
  m
}
