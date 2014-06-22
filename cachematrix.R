##  Below are two functions that are used to create a special object 
##  that stores a square matrix and cache's its inverse.

##  This function creates a special "matrix" object that can cache its inverse.
##  This is really a list containing a function to 
##  1. Set the value of the Matrix
##  2. Get the value of the Matrix
##  3. Set the value of the Inverse
##  4. Get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- null
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned
##  by makeCacheMatrix above. If the inverse has already been calculated
##  (and the matrix has not changed), then it retrieves the inverse from
##  the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)){
                message("Getting cached data")
                return inv
        }
        data <-x$get
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}
