## This code provides two functions.
## The first is makeCacheMatrix, which stores a matrix and its inverse
##
## The second, cacheSolve, 

## Takes in a matrix and stores it elsewhere
## Also stores a given inverse of the matrix in a variable
## Appropriate setters and getters for the matrix and inverse are provided

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Takes in a stored matrix and checks to see if the inverse is 
## already stored. If so, returns the inverse. If not, calculates 
## and returns the inverse, while storing it for future use.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
