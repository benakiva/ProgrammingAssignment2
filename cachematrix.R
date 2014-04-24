## This function creates a special "vector" containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize the stored matrix inverse value to NULL
    mtxInv <- NULL
    
    set <- function(y) {
        x <<- y
        mtxInv <<- NULL 
    }

    get <- function() x
    
    setMatrixInverse <- function(inv) mtxInv <<- inv
    getMatrixInverse <- function() mtxInv

    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)   
}

## This function retrieves the matrix inverse value
## from the cache if it exits. In case the inverse of
## a matrix does not exist in the cache, the function
## calculates it and store the result in the cache and
## returns the value calculated.
cacheSolve <- function(x, ...) {
    # check if the inverse is already cached
    mtxInv <- x$getMatrixInverse()
    
    # if the matrix inverse exits in the cache,
    # we return it here
    if(!is.null(mtxInv)) {
        message("getting cached matrix inverse")
        return(mtxInv)
    }
    
    # if the matrix inverse isn't in the cached, 
    # we get the matrix into data
    message("matrix inverse not in the cache. Computing it...")
    data <- x$get()
    
    # compute the matrix inverse
    mtxInv <- solve(data, ...)
    
    # set in to the cache the computed inverse
    x$setMatrixInverse(mtxInv)
    
    ## Return a matrix that is the inverse of 'x'
    mtxInv
}
