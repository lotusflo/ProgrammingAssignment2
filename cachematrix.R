## Function to set and get the inverse of a matrix. 
## If the contents of a matrix are not changing, teh value is cached so that its retrieved from cache instead
## of computing the inverse again.
## These functions use solve to compute the inverse.

## This function assumes that the matrix supplied is always INVERTIBLE.

## The makeCacheMatrix function will be used to create a special "matrix" and will store the 
## matrix and cache's the inverse. (This is a list containing functions : set -> to set the value of matrix,
## get -> to get the calue of matrix
## setinverse -> set the value of the inverse
## getinverse -> get the cached inverse

makeCacheMatrix <- function(x = matrix()) {
   minverse <- NULL
    set <- function(y) {
        x <<- y
        minverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) minverse <<- inverse
    getinverse <- function() minverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## CacheSolve function computes the inverse using solve 
## It first checks if the inverse is already computed and cached
## If exists returns the data from the cache and exits
## Else calculates the inverse and sets using setinverse function as to be
## retrieved later

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- data %*% solve(data, ...)
    x$setinverse(m)
    m        
}
