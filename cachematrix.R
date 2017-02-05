## Function makeCacheMatrix() creates a special "matrix" object 
## that stores a matrix and it's inverse. 
##
## makeCacheMatrix() creates a special "matrix"
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve() calculates the inverse of the "special" 
## matrix created with makeCacheMatrix(). 
##
## However to save time it first checks to see if the inverse has
## already been calculated. If so, it gets the cached inverse and
## skips the computation. Otherwise, it computes the inverse and 
## sets the value in the cache via makeCacheMastrix$setInverse().
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
