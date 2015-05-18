## This program is written to cache the results of an matrix inverse
## calculation. The "cacheSolve" function is passed a "makeCacheMatrix" 
## function closure which provides a caching mechanism.

## The makeCacheMatrix closure is used to store the matrix and possibly
## the inverse of the matrix. To retrieve or set the (inverse)matrix
## if provides the following functions
##  - set(new_matrix)
##  - get() : returns matrix
##  - setInverse(inverse)
##  - getInverse()  : returns inverse_matrix
##
## Note: The inverse of the matrix is not calculated in this closure!
makeCacheMatrix <- function(matrix = matrix()) {
    inverse_matrix <- NULL
    set <- function(new_matrix) {
        matrix <<- new_matrix
        inverse_matrix <<- NULL
    }
    get <- function() {
        matrix
    }
    setInverse <- function(inverse) {
        inverse_matrix <<- inverse
    }
    getInverse <- function() {
        inverse_matrix
    }
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## cacheSolve is passed an makeCacheMatrix instance as x
## x is then used to retrieve the inverse of the matrix 
## if no inverse was returned by the cache the inverse is 
## calculated and then set in the cache
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(is.null(inverse)) {
            # Only calculate the inverse if the cache 
            # didn't contain the inverse
            inverse <- solve(x$get)
            x$setInverse(inverse)
        } 
        inverse
}