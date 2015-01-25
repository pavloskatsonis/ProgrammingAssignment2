## The following functions create an object that encapsulates a matrix and can
## compute its inverse matrix, storing the computed value in a private cache.
## One functions creates the encapsulating object, the other does the
## computation when needed, or uses the cache when available.

## This function creates a special "matrix" object that can cache its inverse,
## but not calculate it. Its calculation is performed by another function.
## It returns a list of four functions:
## 'set' which changes the matrix,
## 'get' which returns the matrix,
## 'setsolved' which caches data (its intented use is to cache the inverse of
##     the matrix supplied in 'set')
## 'getsolved' which returns the data cached by 'setsolved' (that is the
##      matrix that is the inverse of the one supplied in 'set').
makeCacheMatrix <- function(x = matrix()) {
    ## clear the variable that holds the cached data. By using NULL here the
    ## function cacheSolve can use is.null to see if there is no cached data.
    s <- NULL
    ## function that changes the matrix (clearing the obsolete cached data)
    set <- function(y) {
        ## set the value of the variable that holds the matrix to the new value
        x <<- y
        ## reset the cached data (matrix changed, so the inversed changed, too)
        ## By using NULL here the function cacheSolve can use is.null to see if
        ## there is no cached data.
        s <<- NULL
    }
    ## function that returns the matrix
    get <- function() x
    ## function that caches the inversed matrix to the s variable.
    setsolved <- function(solved) s <<- solved
    ## function that gets the cached inversed matrix from the s variable.
    getsolved <- function() s
    ## return a list of all four functions
    list(set = set, get = get, setsolved = setsolved, getsolved = getsolved)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve retrieves the inverse from cache.
cacheSolve <- function(x, ...) {
    ## get the cached inversed matrix from x
    s <- x$getsolved()

    ## if the inversed matrix is cached, return the cached data
    if(!is.null(s)) {
        ## notify user of cache usage
        message("getting cached data")
        ## return cached inversed matrix
        return(s)
    }

    ## Nothing cached, so the inversed matrix has to be calculated

    ## Get the matrix
    data <- x$get()
    ## Calculate the matrix that is the inverse of 'x'
    s <- solve(data)
    ## Cache the matrix that is the inverse of 'x'
    x$setsolved(s)
    ## Return a matrix that is the inverse of 'x'
    s
}
