## [Put comments here that describe what your functions do]
## Put comments here that give an overall description of what your
## functions do
##

## Write a short comment describing this function
## mackeCacheMatrix is a function which makes a speical matrix object that can cache its inverse. 
## the object is  a list containing a function to
## 1. set themak value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { 
    x <<- y
    i <<-NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve is a function calculates the inverse of the special "matrix" created with makeCacheMatrix function. 
## it first check if the inverse has been calculated. if so it get the mean from the cached and skips the computation. Otherwise, it calcuates the inverse of the matrix and sets the value of inverse in the cache via setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached Inverse Matrix")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}
