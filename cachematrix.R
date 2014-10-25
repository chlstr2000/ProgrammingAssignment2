##This function creates a special "matrix" object that can 
##cache its inverse.
##

##Creates a list of four functions:
##set  - sets the value of the matrix
##get - returns the matrix
##setinverse - caches the inverse of the matrix
##getinverse - returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function takes a matrix and either returns the inverse
## from cache or returns the calculated inverse and saves the 
## inverse in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
