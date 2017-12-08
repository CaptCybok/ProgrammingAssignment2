## This R script gets the inverse of a given matrix by the user

## This function creates a special matrix where it contains the
## inverse matrix of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invCached <- NULL
    set <- function(y) {
        x <<- y
        invCached <<- NULL
    }
    get <- function() x
    setInverse <- function(inMatrix) invCached <<- inMatrix
    getInverse <- function() invCached
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Inverses the given special matrix using the solve function
## If the inverse cache of the given special matrix is not NULL
## then it returns the cached inverse to the user.

cacheSolve <- function(x, ...) {
    invMatrix <- x$getInverse()
    if(!is.null(invMatrix)) {
        message("Retrieving cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInverse(invMatrix)
    invMatrix
    ## Return a matrix that is the inverse of 'x'
}
