## The following functions are used in conjuction to:
## 1. Create a list object that acts as a cache for a matrix and
## its inverse.
## 2. Calculate and return the matrix inverse and cache the inverse.

## This function (makeCacheMatrix) takes a as an argument a matrix
## and creates a list object that can return the original matrix
## or the cached inverse of the matrix once it has been set.

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse to NULL
    inverse <- NULL           
    ## set() re-initializes x to the input and inverse to NULL
    set <- function(y) {      
        x <<- y
        inverse <<- NULL
    }
    ## returns the value of the matrix
    get <- function() {      
        x
    }
    ## sets the value of the inverse matrix cache to the input value
    setinverse <- function(inversematrix) {   
        inverse <<- inversematrix
    }
    ## returns the current value of the inverse cache
    getinverse <- function() {
        inverse
    }
    ## output a list that contains the functions defined in makeCacheMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function (cacheSolve) takes a as an argument a list generated
## by makeCacheMatrix and creates a list object that returns 
## the inverse of the original matrix, from a cache if it already
## exists or from a new calculation.  It also populates the cache with
## the inverse matrix when it computes the inverse.

cacheSolve <- function(x, ...) {
    ## sets the value of inverse to the cached value
    inverse <- x$getinverse()
    ## return the cached value if it is not null
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## return the original matric from the list object
    mtrx <- x$get()
    ## Calculate the inverse of the input matrix
    inverse <- solve(mtrx, ...)
    ## Set the inverse matrix in the cahce
    x$setinverse(inverse)
    ## return the inverse value
    inverse
}

