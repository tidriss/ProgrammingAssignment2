## This file defines two functions, one that creat a special matrix object
## that can cahe its inverse and the other is a function that is called on it
## to solve for the inverse.

## func makeCacheMatrix creates a special matrix object that lets you set its inverse
## and save it.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solved) inverse <<- solved
    getInverse <- function() inverse
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)

}


## func cacheSolve takes as an argumant the special matrix object we defined above
## and returns an inverse of the matrix. It either calls a cached version if available
## or solves for the inverse, save it in cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
