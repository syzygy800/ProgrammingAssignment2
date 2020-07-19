######
##
## the following functions speedup the computation of a matrix inverse
## by using a cache. If the value is needed multiple times it is not recomputed
## every time.
##
######


######
#
# Creates a matrix object which caches its inverse.
#
######
makeCacheMatrix <- function(x = matrix())
{
  # Define the member variables
  inv <- NULL


  # Define the member functions
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinv <- function(invMat) inv <<- invMat
  getinv <- function() inv


  # Construct object and return it
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


######
#
# Returns the inverse of matrix x.
# If it was already calculated the cached value is returned.
#
######
cacheSolve <- function(x, ...)
{
  # Get the cached inverse matrix
  invMat = x$getinv()

  # No cached value?
  if ( is.null(invMat))
  {
    # Compute the inverse and cache it
    invMat = solve(x$get())
    x$setinv(invMat)
  }

  # Return the value (either computed or cached)
  return(invMat)
}
