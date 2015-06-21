## This function creates a special "matrix" object that can cache its inverse
## It assumes that the matrix supplied is always invertible
## It  creates a special "matrix", which is really a list containing
## a function to
## 1. set the the matrix
## 2. get the the matrix
## 3. set the the inverse matrix
## 4. get the the inverse matrix

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  
  # 1. set the the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # 2. get the the matrix
  get <- function() x
  
  # 3. set the the inverse matrix
  setinverse <- function(solve) m <<- solve
  
  # 4. get the the inverse matrix
  getinverse <- function() m
  
  # return a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve retrieves the inverse
## from the cache

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
