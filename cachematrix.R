## makeCacheMatrix creates a list of functions used to set
## and cache a matrix and its inverse. cacheSolve calculates the 
## inverse of a given matrix and creates a cache via the setmean function.

## Creates a list of functions used to set and cache a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Checks cache if the inverse of a given matrix has alreadby been calculated
## and gives the inverse if found. Otherwise, it calculates the inverse of the
## matrix and sets the value of the inverse in the cache.

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