## This is a function to calculate the inverse of a matrix,we are using two 
## functions 1. makecachematrix which creates a matrix and 2. cachesolve which 
##will be inversing the matrix that was created with makecache matrix, 
##if the value is already computed for inverse matrix, cachesolve retrieves 
## that value otherwise it calculates the inverse.
## 

## makecache matrix function will create a special matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cachesolve will be inversing the matrix which is an output from makecache matrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        ## it calculates the inverse matrix -an output from makecache matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        ## if already the cached matrix not available, it calculates inverse
        x$setsolve(m)
        m
}

