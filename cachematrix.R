?invers## 
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Function for creating of list of functions for working with matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                                             ## variable for caching inverse matrix
        
        get <- function()  {x}                                ## get the initial matrix
        setInverse <- function(inverse)  {i <<- inverse}      ## function for caching of calculated matrix
        getInverse <- function() {i}                          ## function to get inverse matrix
        
        list(get = get, setInverse = setInverse, getInverse = getInverse)
        
}


## Function computes the inverse of the special "matrix" returned by makeCacheMatrix above 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    i <- x$getInverse()
    
    if (!is.null(i)){
            message("getting cached data")
            return(i)
    }
    
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    
    i
}
