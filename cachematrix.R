# ------------------------------------------------------------------------------
# Helper functions to compute and cache the inverse of a matrix (i.e. "solve" a
# matrix). Computing the inverse of a matrix can be a time-consuming operation,
# therefore it's useful to be able to cache the computation so the value can be
# used multiple times without running the computation again.
#
# Note: we assume a square matrix (i.e. one that is invertible) is always
# provided when using these functions. Therefore there is no error handling for
# cases where this is not true.
#-------------------------------------------------------------------------------


# Given an initial matrix, this method returns a special wrapper, in the form of
# a list, with functions that can be used to:
# - set the underlying matrix again
# - get the underlying matrix
# - set the inverse of the underlying matrix (which caches this value internally)
# - get the inverse of the underlying matrix (which uses the cached value)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  # Cache for the inverse matrix of `x`

  # Setter method, if we want to overwrite the underlying matrix `x`
  set <- function(y) {
    x <<- y
    i <<- NULL  # Reset the cache
  }

  # Getter method, to access the underlying matrix `x`
  get <- function() {
    x
  }

  # Set the inverse of the underlying matrix `x`, into the cache
  setinverse <- function(inverse) {
    i <<- inverse
  }

  # Get the inverse of the underlying matrix `x`, from the cache
  # Note: this will return `NULL` if the cache hasn't been set yet
  getinverse <- function() {
    i
  }

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Use this method to "solve" (i.e. get the inverse) of a matrix, doing so in a
# way that caches the very first computation of the inverse computation, and
# uses the cache for subsequent access.
#
# The argument `x` MUST be a special wrapper obtained from `makeCacheMatrix`.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # Try to get the cached value
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  # … otherwise compute the inverse and cache it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
