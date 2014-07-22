## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function makeCacheMatrix creates a "matrix" which contains the function to (1) set the value of the matrix (2) get the value of the matrix (3) set the value of the inverse of the matrix and (4) to get the value of the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {

 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## This function cacheSolve calculates the inverse of the matrix created with the above function. But before calculating it checks if the inverse has been calculated already or not. If it is so it gets the cached value instead of re-computing it. If it is not calculated previously it calculates the inverse of the matrix by calling the solve function and sets the value of the inverse via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
