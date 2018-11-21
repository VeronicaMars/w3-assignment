## These two functions avoid duplicate computation on matrix inverse through storing cache value and retrieving when needed

## This function creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function () s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function retieves the cache value that stores in makeCacheMatrix() object's environment

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
            message("getting cached data")
            return (s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
