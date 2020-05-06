## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse property
  i <- NULL
  ## Method to set & get the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  ## Method to set & get the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i

  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##Computes the inverse of the special matrix
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}

