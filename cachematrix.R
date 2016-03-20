## These functions allow you to calculate and save the inverse of a matrix,
## so that R doesn't have to compute repeatedly if you need to calculate it
## several times.

## Creates a matrix, which is a list containing
## a function to:
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL
    set <- function(y) {
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) sol <<- solve
    getsolve <- function() sol
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This functions first checks if the matrix from the getCacheMatrix() 
## function has already been inverted and saved. If so, it returns the 
## saved inverse. If it has not been inverted, then it calculates and 
## stores the inverse of the new matrix.


cacheSolve <- function(x, ...) {
    sol <- x$getsolve()
    if(!is.null(sol)) {
        message("getting cached data")
        return(sol)
    }
    data <- x$get()
    sol <- solve(data, ...)
    x$setsolve(sol)
    sol
}
