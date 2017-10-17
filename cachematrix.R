## Caching the Inverse Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than computing it repeatedly. Below are two functions.
## The functions will store a matirx and cache its inverse. 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
          x <<- y
          invmat <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverse) invmat <<- inverse
        getInverseMatrix <- function() invmat
        list( set = set,
              get = get,
              setInverseMatrix = setInverseMatrix,
              getInverseMatrix = getInverseMatrix)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getInverseMatrix()
        if (!is.null(invmat)) {
          message("getting cached data")
          return(invmat)
        }
        mat <- x$get()
        invmat <- solve(mat, ...)
        x$setInverseMatrix(invmat)
        invmat
}
