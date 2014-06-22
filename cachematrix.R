## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
    # Create a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   x: matrix.
    #
    # Returns:
    #   The special "matrix" object with matrix x.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) 
}


cacheSolve <- function(x, ...) {
    # Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
    # If the inverse has already been calculated (and the matrix has not 
    # changed), then the cacheSolve retrieves the inverse from the cache.
    #
    # Args:
    #   x: Special "matrix" returned by makeCacheMatrix
    #   ...: Passthru arguments for solve function
    #
    # Returns:
    #   Matrix that is the inverse of 'x'.
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If not cached calculate and store in special object
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
