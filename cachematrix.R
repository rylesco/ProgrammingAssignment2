## R Programming Assignment #2
## makeCacheMatrix

## Create a custom matrix that can store a cache of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cacheInverse <- NULL 
    set <- function(y) {
        x <<- as.matrix(y)
        cacheInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) cacheInverse <<- inv
    getInverse <- function() cacheInverse
    list(set = set, get = get, setInverse = setInverse , getInverse = getInverse)
}


## Get the inverse of the matrix created using makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cache data")
        return (inv)
    }
    ret = x$setInverse(solve(x$get()))
    ret
}
