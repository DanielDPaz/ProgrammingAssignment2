## Create and use special matrix object capable of caching its inverse

## This function creates a list object with function values to set the 
## underlying matrix, retrieve this matrix, set (cache) the inverse of that 
## matrix and retrieve the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setmatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(setmatrix = setmatrix, 
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of the matrix of x which is an instance of the 
## list returned by _makeCacheMatrix_. If the inverse has already be pre-computed
## the cached value is returned and the computation is avoided.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached inverse")
            return(i)
        }
        m <- x$getmatrix()
        i <- solve(m, ...)
        x$setinverse(i) 
        i
}
