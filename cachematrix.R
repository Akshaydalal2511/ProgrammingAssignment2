## Caching the inverse of a Matrix
## First Function Caches the matrix and its inverse
## If the same matrix is asked inverse of second function caches the inverse
## rather than recomputing.

## Caches the matrix and its inverse and returns a list

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(invMat) i <<- invMat
    getinv <- function() i
    
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## CacheSolve first checks if the mean has already been calculated
## If yes than gets the inverse from cache and stops
## If not than computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {   ##Checks if inverse is cached
        message("Getting Cached Inverse Matrix")
        i
    }
    matData <- x$get()
    i <- solve(matData, ...) ##Solves for inverse
    x$setinv(i)
    i
}
