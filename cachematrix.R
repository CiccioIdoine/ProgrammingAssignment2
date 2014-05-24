## Assignment2: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    xInvert <- NULL
    set <- function(y) {
        x <<- y
        xInvert <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) xInvert <<- invert
    getinvert <- function() xInvert
    list(
        set = set
        ,get = get
        ,setinvert = setinvert
        ,getinvert = getinvert
    )
}


## This function returns the inverse of the underlying matrix
## If xInvert is not null, then the data already exists and will be taken from the cache
## Otherwise it will be calcultated accordingly
cacheSolve <- function(x, ...) {
    xInvert <- x$getinvert()
    if(!is.null(xInvert)) {
        message("getting cached data")
        return(xInvert)
    }
    data <- x$get()
    xInvert <- solve(data)
    x$setinvert(xInvert)
    xInvert
}
