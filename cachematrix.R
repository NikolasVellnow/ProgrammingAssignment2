## I coded two functions:
## makeCacheMatrix() caches the inverse of an input matrix
## and creates object of type makeCacheMatrix()
## cacheSolve() calculates the inverse of an input matrix
## from object of type makeCacheMatrix()
## If the inversed matrix has been cached before by makeCacheMatrix,
## cacheSolve() will use the already cached inverse of the matrix

## Creates object of type makeCacheMatrix()
## and caches inverse of input matrix

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Calculates inverse of matrix it retrieves from an object of type makeCacheMatrix(),
## if matrix is invertible

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

