## The cachematrix functions: makeCacheMatrix creates a list of 
## functions that cache a matrix and its inverse, and cacheSolve
## evaluates the inverse only if the inverse has never been calculated

## makeCacheMatrix takes a matrix as a parameter, creates the object inv
## to store the inverse of the matrix, and returns functions
## "get", to retrieve the matrix, "setinv", to assign a value
## to inv, "getinv" to retrieve the value of inv, and "set", to reset
## the value of the matrix and discard any prior calculations of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve first checks the value of the cached inverse to
## see if it has been calculated already, in which case it returns
## that value; if not, it calculates the value, stores it
## in the cache, and returns it

cacheSolve <- function(x, ...) {
  curinv <- x$getinv()
  if (!is.null(curinv)) {
    message("getting cached data")
    return(curinv)    ## Return cached inverse of x
  }
  data <- x$get()
  curinv <- solve(data, ...)
  x$setinv(curinv)
  curinv     ## Return newly calculated inverse of x
}
