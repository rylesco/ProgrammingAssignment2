## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
