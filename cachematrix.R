## This set of functions assign a value to a "matrix 
## object" in the cached memory, i.e. outside the current
## environment.  

## The first function creates the "matrix object" and 
## sets the initial value of that matrix object as NULL.
## It then gets the value of the matrix; 
## sets the value of the matrix;
## sets the value of the inverse of the matrix; 
## and gets the value of the inverse matrix.  

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The second function calculates the inverse of the 
## matrix object created by the first function. If a 
## value of the inverse has previously been calculated
## it gets that value from the cache, otherwise it 
## calculates the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
