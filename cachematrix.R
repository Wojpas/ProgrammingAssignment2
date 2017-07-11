## Below pair of functions cache the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix" object that:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setinv <- function(inv) invM <<- inv
    getinv <- function() invM
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve function computes the inverse of the "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve
## will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    invM <- x$getinv()
    if(!is.null(invM)) {                
        message("Getting cached data") 
        return(invM)                
    }
    data <- x$get()
    invM <- solve(data, ...)
    x$setinv(invM)
    invM
}
