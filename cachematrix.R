## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix , takes a square matrix as an input then
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse
## 5.creates a list to be passed to cachesolve function

makeCacheMatrix <- function(x = matrix()) {
  matrixvalue = NULL
  
  ## set the value for matrix
  set = function(y) {
    x <<- y
    matrixvalue <<- NULL
  }
  
  ## gets its value
  get = function() {
    x
  }
  
  ## sets the value for inverse
  setinverse = function(inverse) {
    matrixvalue <<- inverse
  }
  
  ## gets the value for inverse
  getinverse = function() {
    matrixvalue
  }
  
  ## creates the list for cacheresolve
  list(
    set = set, get = get, setinverse = setinverse, getinverse = getinverse
  )
  
}


## Write a short comment describing this function
## takes the list from makeCacheMatrix function and calcultes the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## gets the inverse value from the diff environment
  matinv = x$getinverse()
  
  ## if it is not null , then return the cache
  if (!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  ## else calculate the inverse and store it in the cache
  else {
    matrixdata = x$get()
    matinv = solve(matrixdata, ...)
    x$setinverse(matinv)
    return(matinv)
  }
  
}
