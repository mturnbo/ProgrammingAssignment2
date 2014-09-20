## makeCacheMatrix creates a matrix object that caches it's own inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(solve) m <<- solve
  
  getInverse <- function() m
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## cacheSolve computes the inverse of the matrix object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Retrieving cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
