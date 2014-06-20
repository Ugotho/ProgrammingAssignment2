## The following functions, makeCacheMatrix() and cacheSolve(), 
## expedite work with symmetric matrices and their inverses by 
## utilizing a "cacheMatrix" object. Such an object not only holds 
## the original matrix but also can cache the matrix inverse so the  
## latter object only needs to be calculated once but can be reused over 
## and over without repeating the calculation.

## This function generates a "cacheMatrix" object that not only  
## contains a symmetric matrix but can also hold a cached copy of 
## the inverse of that matrix 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes a "cacheMatrix" object (x) and checks to 
## see if it contains a cached value for the inverse of that matrix. 
## If there is no cached inverse present, this function calculates 
## the inverse and caches it within the object (x). 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    else {
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
    }
    i
}
