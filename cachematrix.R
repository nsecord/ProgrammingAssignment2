## This makeCacheMatrix function is used to create
## a caches to store the matrix x and its inverse minv.
## 
## It is used in conjunction with cacheSolve() which
## solves for the inverse of the matrix and uses the
## setInverse() function to store the result in the cache. 
## 
## makeCacheMatrix uses lexical scoping to assign and
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
	    ## Initiate the matrix inverse minv to NULL
	    minv <- NULL
	    ## Setter function for setting the stored matrix x
        set <- function(y) {
            x <<- y
            minv <<- NULL
        }
        ## Getter function for retrieving the stored matrix x
        get <- function() x
        ## Setter function for the matrix inverse of x
        setInverse <- function(inverse) minv <<- inverse
        ## Getter function for the matrix inverse of x
        getInverse <- function() minv
        ## Create a list of available functions so that when the user
        ## performs x$elem they get the result of the appropriate
        ## setter or getter function defined by elem.
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
        	## If the result stored in the cache is not null, 
        	## print a message to the console saying we are 
        	## using the cached data and return the retrieved
        	## result.
            message("getting cached data")
            return(inv)
        }
        ## Otherwise, solve for the inverse using the solve() 
        ## function and set the value in the cache using the 
        ## setInverse() function.
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
