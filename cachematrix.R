## Put comments here that give an overall description of what your
## functions do

## These 2 functions are written for 'Programming Assignment 2' of the Coursera course: R Programming.
## Week 3 Assignment; February 23, 2020; GitHub user: mielcnuts


## Write a short comment describing this function

## The function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.

## The <<- operator is used to assign a value to an object in an environment which is different from the current environment.  

makeCacheMatrix <- function(x = matrix()) {   ##argument is defined as default value 'matrix'
    inv <- NULL                               ##initialize inv as NULL
    set <- function(y){                       ##the 'set function' will store the new matrix
      x <<- y
      inv <<- NULL                            ##if there is a new matrix, reset inv to NULL
    }
    get <- function() x                      
    setInverse <- function(solveMatrix) inv <<- solveMatrix ##assigns value of inv in parent environment
    getInverse <- function() inv                            ##gets the value of inv where called
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
