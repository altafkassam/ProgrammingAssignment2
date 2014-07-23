## The following pair of functions will calculate the inverse of a
## matrix and store it in cache. Subsequent calls to cacheSolve will
## return the cached inverse if it was calculated previously

##  makeCacheMatrix This function creates a special "matrix" object
##  that can cache its inverse. It can also calculate the inverse
##  of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    getinverse <- function(matrix) solve(x)
    setMatrix <- function(matrix) m <<- matrix
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getinverse = getinverse,
         getMatrix = getMatrix)
}


## cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    m
}
