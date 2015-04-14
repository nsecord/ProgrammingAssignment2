## This makeCacheMatrix function is used to create
## a caches to store the matrix x and its inverse minv.
## 
## It is used in conjunction with cacheSolve() which
## solves for the inverse of the matrix and uses the
## stores the result int the cache. 
## 
## The function uses lexical scoping to assign and
## search for the cached variables in the parent 
## environment of where the functions are called.
## 
## Example Usage:
##
##   > m <- matrix(c(-1, -2, 1, 1), 2,2)
##   > x <- makeCacheMatrix(m)
##   > x$get()
##        [,1] [,2]
##   [1,]   -1    1
##   [2,]   -2    1
##   > inv <- cacheSolve(x)
##   > inv
##        [,1] [,2]
##   [1,]    1   -1
##   [2,]    2   -1
##   > inv <- cacheSolve(x)
##   getting cached data
##   > inv
##        [,1] [,2]
##   [1,]    1   -1
##   [2,]    2   -1
##

makeCacheMatrix <- function(x = matrix()) {
	    minv <- NULL
        set <- function(y) {
            x <<- y
            minv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) minv <<- inverse
        getInverse <- function() minv
	    list(set = set, get = get,
	         setInverse = setInverse,
	         getInverse = getInverse)

}


## The function cacheSolve() uses a cache and the solve
## function to return the inverse of a matrix x.
##
## If this is the first time to solve for the inverse
## of the matrix x using cacheSolve(), the solve() function
## is used to find the inverse and then the result is stored
## in cache using the setter function of makeCacheMatrix.
##
## If cacheSolve(x) has already been used once to solve for 
## the inverse of the matrix x then that inverse is
## retrieved from the cache using the getter function
## of makeCacheMatrix and the result returned to the user.
##

cacheSolve <- function(x, ...) {
        ## Check to see if the inverse has already been
        ## calculated.
        inv <- x$getInverse()
        if(!is.null(inv)) {
        	## If the inverse is not null, return the
        	## result retrieved from the cache.
            message("getting cached data")
            return(inv)
        }
        ## Otherwise, it solves for the inverse using 
        ## the solve() function and sets the inverse
        ## in the cache using the setInverse() function.
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
