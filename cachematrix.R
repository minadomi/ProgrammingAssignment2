## These functions are used for performing the time-intensive operation of 
## calculating the inverse of a matrix and cache the value so that it does
## not have to be computed again.

## Pass a matrix into the makeCacheMatrix function and store the output.
## The output will be a list of functions.
## Example: bar <- makeCacheMatrix(foo)
## to read the matrix from bar, use bar$get()
## to read the inverse of the matrix, if already cached, use bar$getsolution()
## to cache the inverse "baz" of the matrix, use bar$setsolution(baz).

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolution <- function(solution) s <<- solution
        getsolution <- function() s
        list(set = set, get = get, setsolution = setsolution, getsolution = getsolution)
}


## Use cacheSolve on the output "bar" from makeCacheMatrix to calculate the inverse
## of the matrix and cache it. If the inverse has already been calculated, 
## cacheSolve simply retrieves it from the cache. 

cacheSolve <- function(x, ...) {
        s <- x$getsolution()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolution(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
