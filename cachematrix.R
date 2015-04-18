## Coursera R Programming course - Assignment 2
## Functions allow avoiding duplicated computation of
## inverse matrices by caching previous results

## Wraps a matrix object in a caching layer.
## The returned object contains set/get/setinverse/getinverse
## methods that allow cache manipulation.
makeCacheMatrix <- function(x = matrix()) {
    # inv - cache variable set in parent frame
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- null
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    invisible(list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse))
}


## Solves a given matrix. The computation is speeded up by
## using a cached result if available.
cacheSolve <- function(x, ...) {
    inv = x$getinverse()
    if (!is.null(inv)) {
        message("Returning inverse matrix from cache")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
