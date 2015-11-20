# This pair of functions cache the inverse of a matrix

## First, makeCacheMatrix creates a special matrix object that can cache its inverse
## makeCacheMatrix is a function that stores a list of other functions
#Example Matrix for testing
# B = matrix( 
#      c(2, 4, 3, 1, 5, 7), 
#      nrow=3, 
#     ncol=2) 

#Really do not need set function, but left in to follow example

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


## Second, cacheSolve computes the inverse of the special matrix returned
## by makeCacheMatrix
## Run cacheSolve(makeCacheMatrix(B)) to solve where B is a matrix 

cacheSolve <- function(x, ...) {
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

