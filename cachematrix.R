## makeCacheMatrix creates a special list containing a function to
##1.set the value of the vector
##2.get the value of the vector
##3.set the value of the mean
##4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) m <<- inverseMatrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function return Inverse matrix. 
##It first checks to see if the inverse has already been calculated
##If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates inverse matrix and sets it.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
