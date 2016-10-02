## These are two functions that are used to create an object that 
## stores a matrix and caches its inverse.

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(yy = matrix()) {
        inverse <- NULL
        set <- function(y) {
                yy <<- y
                inversed <<- NULL
        }
        get <- function() yy
        setInverse <- function(inversed) inverse <<- inversed
        getInverse <- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Here below the function computes the inverse of the "matrix" created by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(yy, ...) {
  ## Return a matrix that is the inverse of 'yy'
        inverse <- yy$getInverse()
        if (!is.null(inverse)) {
                return(inverse)
        }
        zzz <- yy$get()
        inverse <- solve(zzz, ...)
        yy$setInverse(inverse)
        inverse
}

