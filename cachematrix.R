## The two functions makeCacheMatrix and cacheSolve are used to
## calculate the inverse of a matrix caching the results.

## Being x an invertible matrix, makeCacheMatrix(x) function returns
## a list containing the functions to:
## 1. set: set the value of x, 2. get: get the value of x
## 3. setSolve: set the inverse of x, 4. getSolve: get the inverse of x

makeCacheMatrix <- function(x = matrix()) {
  if(nrow(x)!=ncol(x) | det(x)==0) {
    stop("Must be an invertible matrix")
  }
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) n <<- solve
  getSolve <- function() n
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## Being z the result of makeCacheMatrix(x),
## cacheSolve(z) function returns the inverse of x.
## If the inverse of the matrix has been calculated before
## it returns the cached result insead of calculating it.

cacheSolve <- function(z, ...) {
  m <- z$getSolve()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- z$get()
  m <- solve(data, ...)
  z$setSolve(m)
  m
}
