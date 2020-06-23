## The following functions are used for calculating the 
## inverse of an invertible matrix.The matrix may be large,
## so the inverse matrix is stored in a cache where it can
## be retrieved later, rather than needing to recompute it.
## If the inverse matrix is not retrieved, then it is 
## calculated.


## makeCacheMatrix takes an invertible matrix and creates a "special" vector
## (a list of 4) containing functions to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) v <<- solve
        getinverse <- function() v
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve calculates the inverse of the "special" matrix
## returned by makeCacheMatrix. If the inverse matrix has 
## already been calculated then cacheSolve retrieves it 
## from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        v <- x$getinverse()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinverse(v)
        v
}
