## The following two functions are used to calculate the inverse of a matrix.
## Instead of calculating the inverse by directly usint the solve function,
## first we use the function "makeCacheMatrix" to create a list which stores
## the matrix and its cached inverse. Then, when the function "cacheSolve" is
## called, it first checks the list to see if the inverse has already been
## calculated. If yes, it simply returns it, if not, calculates it, stores it
## in the list and returns it.

## Function name: makeCacheMatrix
## arguments: a matrix named x [default value matrix()]
## returns: a list containing 4 functions:
### - function set: sets the value of the matrix
### - function get: gets the value of the matrix
### - function set.inv: sets the value of the inverse matrix
### - function get.inv: gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() { x }
    set.inv <- function(inverse) { inv <<- inverse }
    get.inv <- function() { inv }
    list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}


## Function name: cacheSolve
## arguments: a list returned by makeCacheMatrix
## returns: the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get.inv()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set.inv(inv)
    inv
}
