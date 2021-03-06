## Perform matrix inversion, and cache the result to reduce potentially costly computation on repeated requests for the inverse.

## The first function, makeCacheMatrix, creates a special "matrix", which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix
## NOTE: It is assumed that the matrix is invertible.
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

## The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data (using the 'solve' function) and sets the value of the inverse in the cache via the setsolve function of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

#TEST CASE: 
# > y <- matrix(data=c(3,3.5,3.2,3.6),nrow=2,ncol=2)
# > mmm <- makeCacheMatrix(y)
# > mmm$get()
#      [,1] [,2]
# [1,]  3.0  3.2
# [2,]  3.5  3.6
# # First call computes.
# > cacheSolve(mmm)
#       [,1] [,2]
# [1,] -9.00  8.0
# [2,]  8.75 -7.5

# # Second and subsequent call pulls from the cache.
# > cacheSolve(mmm)
# getting cached data
#       [,1] [,2]
# [1,] -9.00  8.0
# [2,]  8.75 -7.5
