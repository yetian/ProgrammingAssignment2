## An R function that is able to cache potentially time-consuming computations


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  # member
  inverse_mtx <- NULL
  
  # mtx
  set <- function(x) {
    mtx <<- x
    inverse_mtx <<- NULL
  }
  get <- function() mtx
  
  # inverse
  set_invmtx <- function(inv_mtx) {
    inverse_mtx <<- inv_mtx
  }
  get_invmtx <- function() inverse_mtx
  
  # return
  list(set = set, get = get, set_invmtx = set_invmtx, get_invmtx = get_invmtx)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtx <- x$get_invmtx()
  
  # if inverse matrix is cached
  if(!is.null(mtx))
  {
    message("getting cached data")
    return (mtx)
  }
  
  # else
  data <- x$get()
  invx <- solve(data, ...)
  x$set_invmtx(invx)
  invx
}
