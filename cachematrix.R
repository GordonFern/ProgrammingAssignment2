## Put comments here that give an overall description of what your
## functions do
## Solution consists of two functions:
## 1. makeCacheMatrix - creates a special matrix 
## 2. cacheSolve - takes in a matrix created by makeCacheMatrix as a parameter and solves it.
##                 if the solution has already been solved then returned the cache results


## function makeCacheMatrix(matrix) (optionally takes an initial matrix)
##
## Function that contains member functions
## $get() - returns the currently stored matrix class(matrix)
## $set() - sets a new matrix taking as input a matrix class(matrix)
## $setinv(invmatrix) - sets the Inverse matrix to the parameter invmatrix. NB. It does
##                      not solve the matrix
## $getinv() - returns the currently stored inverse matrix as set by $setinv or NULL if it
##             has yet to be set

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invmatrix) inv <<- invmatrix
  getinv <- function() inv
  list( set=set,get=get,setinv=setinv,getinv=getinv)

}

## function cacheSolve(special_matrix)
##
## Takes as a parameter a matrix created by makeCacheMatrix
## and returns the inverse matrix.
##
## If the matrix has already been solved (inverse matrix already calculated) then the cached
## results are returned rather than re-solve the matrix.

cacheSolve <- function(x, ...) {
  
  inv<-x$getinv()
  if( !is.null(inv)) {
     message("getting cached data")
     return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv
}
