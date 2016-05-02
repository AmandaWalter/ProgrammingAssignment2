## This script will find the inverse of a matrix and store it in memory to be called more quickly in the future.

## This function will create a list of operations that can be used to store and retreive the cached matrix
makeCacheMatrix <- function(x = matrix()) {
    memInverse <- NULL
    set <- function(y){
        x <<- y
        memInverse <<- NULL
    }
    get <- function() x
    setmatrix <- function(I) memInverse <<- I
    getmatrix <- function() memInverse
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## This function will return an inverse matrix of x.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    memInverse <- x$getmatrix()
    if(!is.null(memInverse)) {
        message("Retreiving stored inverse matrix")
        return(memInverse)
    }
    sq_matrix <- x$get()
    memInverse <- solve(sq_matrix)
    x$setmatrix(memInverse)
    memInverse
}