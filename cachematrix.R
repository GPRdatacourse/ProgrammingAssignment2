## CCoursera Course: R_Programming
## Programming Assignment 2: Lexical Scoping

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ##sets the watch variable to NULL
  ## the following lines set a series of 4 functions (set, get, getinverse and setinverse) that can be called
  ## to control how the caching can be controlled
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

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will return the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## check to see if the inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## calculate the inverse
  m <- solve(data, ...)
  x$setinverse(m)
  m
}