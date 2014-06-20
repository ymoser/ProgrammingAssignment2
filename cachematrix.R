## A pair of functions to cache the inverse of a matrix 
## rather than computing it repeatedly

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix", or returns
  ## it from the cache if it has already been calculated
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  ## Return a matrix that is the inverse of 'x'
  
}