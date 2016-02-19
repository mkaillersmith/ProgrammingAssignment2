## Makes Cache Matrix and Solves for inverse of Matrix

## Creates a Matrix and Caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) 
                m <<- solve()
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the matrix, but if matrix inverse is already cached then it returns the cached inverse matrix

cacheSolve <- function(x, ...) {
        m < x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(m)
        m
}
