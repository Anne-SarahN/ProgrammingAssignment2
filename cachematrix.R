## Overall description of what these functions do:

## These two functions work together to create a list object which
## is able to store a matrix and its inverse, and to either just
## give it back when needed, if the matrix to be inverted did not change,
## or to recompute the inverse, store it, and of course give it back

## This function creates the list object capable of storing a matrix
## and its inverse, and of giving out one or the other when requested
## The 'set' and 'setInv' elements of the list are the functions storing
## the matrices, and the 'get' and 'getInv' elements are the functions
## giving out the values. When 'set'-ting the matrix, its inverse is
## cleared out.

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    getInv <- function() xInv
    setInv <- function(invM) xInv <<- invM
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function uses the object created with makeCacheMatrix() to
## return the inverse of a matrix, either already cached, or freshly
## computed (and stored for future requests)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInv <- x$getInv()
    if(!is.null(mInv)) {
        return(mInv)
    }
    x$setInv(solve(x$get()))
    x$getInv()
}
