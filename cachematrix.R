## Below are two functions that are used to create a special object that 
## stores a matrix and caches its inverse

##  The first function, makeCacheMatrix creates a special "matrix" object that
##  can cache its inverse.
##  "matrix" object is really a list containing a function to 
##  1.set the value of the matrix
##  2.get the value of the matrix
##  3.set the value of the inverse
##  4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}

## The following function cacheSolve  computes the inverse of the special
## "matrix" created with the above function makeCacheMatrix. If the inverse 
## has already been calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
