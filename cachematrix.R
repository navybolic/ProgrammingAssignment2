## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of a matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv_x <<- inverse
    getInverse <- function() inv_x
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The function cacheSolve returns the inverse of a matrix 'A' created with
## the above makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, if not,
## it computes it, caches it, and returns it.

cacheSolve <- function(x, ...) {
    ## Returns a matrix inverse of 'x'
    inv_x <- x$getInverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse of matrix...")
        return(inv_x)
    } 
    inv_x <- solve(x$get())
    x$setInverse(inv_x)
    return(inv_x)
}
