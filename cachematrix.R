## The following functions cache the inverse of a matrix.
## This can be useful when inverse computation is costly and
## the matrix does not change often.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse. It returns a list of functions to
## - set the matrix
## - get the matrix
## - set the cached inverse
## - get the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL  # reset the cache if the matrix changes
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then it retrieves the inverse from
## the cache. Otherwise, it calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}

