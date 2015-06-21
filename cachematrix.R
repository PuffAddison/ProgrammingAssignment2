## Code for Programming Assignemnt 2 of the R Programming course. June 2015
## Two functions to support the use of matrices but avoids the overhead of repeated computation of the inverse of a matrix.
## The approach makes use of the R lexical scoping rules.

## The first function makeCacheMatrix() creates an object  that will store both a matrix and its inverse together. 
## Calculation of the inverse is postponed until it is first required, the function cachSolve() will do this.

## The object consists of a list of four functions whose environment includes the marix and its inverse.
##   1. The function set() which records a new  matrix.
##   2. The function get() that returns the matrix.
##   2. The function setInverse() that records the inverse of the matrix.
##   3. The function getInverse() that returns the inverse of the matrix or NULL if it has yet to be calculated.
## NOTE: It is assumed that any matrix used is invertible.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y=matrix()){
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setInverse<- function(y){
      inv <<- y
    }
    getInverse <- function() inv

     list(set=set, get=get,setInverse=setInverse, getInverse=getInverse)
}


## This function expects a matrix object created by makeCacheMatrix and returns the inverse of the matrix.
## If the inverse has already been calculated it returns the cached inverse.
## If the inverse has not alreay been found it calcutates the inverse, caches it and returns the inverse.
##
## The first argument x is a matrix created by makeCacheMatrix.
## The remaining arguments will be passed to solve() if the inverse needs to be calculated.
## The function uses the solve function of R to calculate the inverse.

cacheSolve <- function(x, ...) {
    ## Get the inverse and if it is is not NULL return it.
    inv <-x$getInverse()
    if(!is.null(inv)) return(inv)
   
    ## Otherewise the inverse has not yet been calculated so  we do so then cache it.
    m <-x$get()
    inv <- solve(m, ...)
    x$setInverse(inv)
    inv
}
