##The functions below can be used to cache the inverse of a matrix.


##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invmat <<- inverse
  getInverse <- function() invmat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat, ...)
  x$setInverse(invmat)
  invmat
}