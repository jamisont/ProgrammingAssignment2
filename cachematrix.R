## These functions create a special object that stores a square matrix
## and caches its inverse

## The following function creates a "matrix" (list) that contains functions to
## 1) set the value of the matrix;
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

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
         setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. It first checks whether the inverse
## has already been calculated. If so, it uses the cached value and skips
## calculation. If not, it calculates the inverse and stores in cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
