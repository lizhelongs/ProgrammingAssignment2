## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## intialization
    inv <- NULL
    ## set matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get matrix
    get <- function() x
    ## set the inverse matrix for x
    setInverse <- function(inverse) inv <<- inverse
    ## get the inverse matrix of x
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    ## if the matrix has been cached then return it
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    ## get the matrix
    data <- x$get()
    ## compute the inverse matrix
    inv <- solve(data, ...)
    ## cache the inverse matrix
    x$setInverse(inv)
    inv
}
