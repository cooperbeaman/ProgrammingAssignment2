## The following functions first create an object that stores an input 
## matrix, and then cache its inverse.

## Function 1: makeCacheMatrix creates a special "matrix", which is really a 
## list containing a function to:
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function 2: cacheSolve computes the inverse of the special "matrix" created 
## above. However, if the inverse has already been calculated (and the matrix 
## remains the same), cachesolve retrieves the inverse from the cache.

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
