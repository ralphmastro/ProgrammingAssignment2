## This file contains two functions:
## One function takes a matrix and creates four functions based on that
## matrix
## The second function creates the inverse of that matrix if that matrix
## isn't already cached.

## Take a matrix as input, return 4 functions related to caching the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setINV <- function(z) m <<- z
    getINV <- function() m
    list(set = set, get = get,
         setINV = setINV,
         getINV = getINV)
}


## Take the result of the above function and 1) check if inverse is already stored,
## if so return it.  If not, calculate the inverse and return that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getINV()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setINV(m)
    return(m)
}

# test the procedure

M <- matrix(c(2, 0 , 0 , 0 , 2, 0, 0, 0, 2),nrow=3,ncol=3)
z1 <- makeCacheMatrix(M)
cacheSolve(z1)
cacheSolve(z1)

