## For cache matric and cache solve function

## Cache Matrix Function

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invr <<- inverse
        getinverse <- function() invr
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Cache Solve Function

cacheSolve <- function(x, ...) {
        invr <- x$getInverse()
        if (!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        mat <- x$get()
        invr <- solve(mat, ...)
        x$setInverse(invr)
        invr
}