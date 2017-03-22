
##  The following functions work with a special kind of square matrix
##  that is invertible (ie. when the matrix is multiplied by its inverse
##  the result is the identity matrix)
##  See also... https://en.wikipedia.org/wiki/Invertible_matrix
 
##  Since the solve function to compute the inverse can be time consuming
##  it makes sense to cache that information in case it needs to be
##  retrieved again later.


##  The makeCacheMatrix is a function of functions to store a matrix
##  It will also store the inverse matrix when cacheSolve is called

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



##  The cacheSolve function will first check if the inverse matrix
##  has already been computed, and if so display it.
##  If not already solved, compute the inverse and save the results
##  and display.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

