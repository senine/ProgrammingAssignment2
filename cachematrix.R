## makeCatheMatrix provides a structure to cache and manupulate the assigned matrix
## and its inverse.
## cacheSolve caculates the inverse function (if NULL) or reads inverse from
## makeCatheMatrix.

## CAUTION: no input check for matrix, assuming inputs are all square and inversable matrices 

## example:
# x <- matrix(1:4, nrow = 2)
# c <- makeCacheMatrix(x)
# cacheSolve(c)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function (y) {
        x <<- y
        inverse <<- NULL # clear the cached inverse since new value of matrix is assigned
    }
    get <- function() x
    set.inv <- function(inv) inverse <<- inv # set inverse manually (unrecommended)
    get.inv <- function() inverse
    
    list(set = set, get = get,
         set.inv = set.inv,
         get.inv = get.inv)
}



cacheSolve <- function(x, ...) {
    inverse <- x$get.inv()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set.inv(inverse)
    inverse
}




