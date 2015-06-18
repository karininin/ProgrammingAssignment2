## Functions that stores a matrix and also caches its inverse in a "matrix" 
## object, so if the inverse has already been calculated, it gets read from 
## cache

## makeCacheMatric creates a list of functions for finding/storing both the
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y                     ##sets the matrix to given values, 
    m <<- NULL                  ##and removes any stored inverse
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,    ##creates a list of function for getting and setting 
       setinverse = setinverse, ##matrix/inverse
       getinverse = getinverse)
}


## checks the cachematrix if the inverse is already cached, otherwise caches 
## it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {                ##check if inverse is already stored
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)            ##otherwise, evaluate inverse
  x$setinverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
