## These functions are used to make an object that stores a matrix
## given by the user and can also cache its inverse.

## The first function creates the special matrix object, and is
## really a list containing functions to set and get the
## value of the matrix given. It also contains functions to set
## and get the value of the inverse of the matrix.
## The inverse is cached in the matrix object under 'm', and can
## be retrieved by 'object'$getinverse.
## A new object can also be set without calling the function again
## by calling 'object'$set(new_matrix). A potentially cached 'm'
## from the previous matrix will be set to NULL again.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrixobject made
## by the makeCacheMatrix function. To save time, it first checks
## whether the inverse has already been calculated and saved in
## the matrix object. It does so in the first part and if there
## is indeed a cached inverse (if 'm' from the previous function)
## exists, it will return this value.
## If 'm' is NULL, the function will calculate the inverse and
## before returning it, it will cache it in the special matrix
## object so that it can be accessed faster later.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
