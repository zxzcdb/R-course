## This file consists of two functions.
## function makeCacheMatrix and function cacheSolve

## makeCacheMatrix creates a list containing a function to

## 1.set the value of the Matrix
## 2.get the value of the Matrix
## 3.set the value of the inverse Matrix
## 4.get the value of the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y){
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinverse <- function(in2) inv <<- in2
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## makeCacheMatrix creates the  inverse of the matrix created with the above function. 
## It first checks to see if the inverse has already been created.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
