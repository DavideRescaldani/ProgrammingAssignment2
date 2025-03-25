# makeCacheMatrix creates a special matrix object that can store its inverse, providing functions to set and get the matrix and its cached inverse.

makeCacheMatrix <- function(X = matrix()){
  inv <- NULL
  set <- function(Y){
    X <<- Y
    inv <<- NULL
  }
  get <- function() X
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve computes the inverse of the matrix created by makeCacheMatrix, retrieving the cached inverse if it has already been calculated to avoid redundant computations.

cacheSolve <- function(X, ...) {
  inv <- X$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- X$get()
  inv <- solve(data, ...)
  X$setinv(inv)
  inv
}
