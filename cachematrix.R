# The following function, creates a special "matrix" object that can cache its inverse. 
# It contains a function to set the value of the matrix, to get the value of the matrix, 
# to set the value of the inverse and to get the value of the inverse.

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(set = set, get = get,
               setInv = setInv,
               getInv = getInv)
        }

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated, 
# then cacheSolve should retrieve the inverse from the cache. 
# Otherwise, it calculates the inverse of the matrix and sets 
# the value in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
        }






