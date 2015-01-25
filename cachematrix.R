## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## Note: It is assumed that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv<- function(solve) m <<- solve ## m will store the inverse of x
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##              If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##              retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        cachem <- x$getinv()
        if(!is.null(cachem)) {
                message("getting cached data")
                return(chachem)
        }
        matrix <-x$get()
        m <- solve(matrix) ##compute inverse of matrix since it's not in cache
        x$setinv(m) ## assign the inverse of matrix to the cache
        m
}
