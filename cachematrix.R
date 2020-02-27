## Caching Inverse of a Matrix

## A matrix with functions to get/set value & get/set inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## first cached inverse of matrix, then define getter/setter for matrix, getter/setter for matrix inverse
## return list of functions for matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache inverse
  x$setinv(inv)
  
  # return inverse of matrix
  return(inv)
        ## Return a matrix that is the inverse of 'x'
}
