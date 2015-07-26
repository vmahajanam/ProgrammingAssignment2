## Following functions accomplish the task of taking the inverse of a matrix from cache if it is present in cache
## otherwise calculate the inverse of the matrix and store it in the cache. The assumption is that the input matrix
## is invertible

## Following function makeCacheMatrix creates a list containing following functions:
##1. set the matrix to be inverted
##2. get the matrix to be inverted
##3. set the inverse of the matrix
##4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##The following function cacheSolve calculates the inverse of the matrix created with the above function. 
##However, it first checks to see if the inverse is already present in the cache. 
##If so, it gets the inverse from the cache and skips the computation.  

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
}
