## This function will cache the inverse of a matrix in order to avoid taking
## extra time to do something we've already calculated

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # setting the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # getting the matrix
    get <- function() x
    ## setting the inverse
    setInverse <- function(inverse) m <<- inverse
    ## getting the inverse
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ## check if it has already been calculated
    if (!is.null(m)){
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
