## Assignment 2: Caching the Inverse of a Matrix
### 1.  `makeCacheMatrix`: This function creates a special "matrix" object
###     that can cache its inverse.
### 2.  `cacheSolve`: This function computes the inverse of the special
###     "matrix" returned by `makeCacheMatrix` above. If the inverse has
###     already been calculated (and the matrix has not changed), then
###     `cacheSolve` should retrieve the inverse from the cache.

## makeCacheMatrix
### Description
#### makeCacheMatrix creates a object using input matrix and reverts with list of functions to set, get matrix and its inverse
### Usage
#### makeCacheMatrix(M) initializes object using input matrix M
#### M$get() retrieve current value of matrix cached
#### M$set(M1) set current value of matrix to new matrix y; Reset inverse of matrix if y is different than x
#### M$getinverse() retrieve inverse of cached matrix
#### M$setinverse(inv) set current value inverse of matrix
### Arguments
#### M, M1          numeric square matrix (invertible)
#### inv            inverse of matrix using solve()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             # Initialize inverse to NULL for initial call to constructor function makeCacheMatrix()

    set <- function(y) { 
        # Check if input argument is identical to original matrix; assign new matrix and reset inverse to NULL if matrix has changed
        if (!identical(x,y)) {
            x <<- y         # Assign matrix to input argument (as cached variable)
            inv <<- NULL    # Initialize inverse to NULL if argument matrix is changed (as cached variable)
        }
    }
    get <- function() x             # Retrieve value of matrix (will retrieve from cache)
    setinverse <- function(inverse) inv <<- inverse # Assign value of inverse to input argument (as cached variable)
    getinverse <- function() inv    # Retrieve value of inverse (will retrieve from cache)
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve
### Description
#### cacheSolve returns the inverse of matrix created in object makeCacheMatrix. It speeds up the process of matrix
#### inversion by caching the inverse if matrix inverse has already been calculated. It assumes that the matrix is
#### is invertible.
### Usage
#### cacheSolve(M)
### Arguments
#### M          "special" matrix (list) returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()   # Retrieve value of inverse using getinverse() method
    
    # Note that if matrix has changed (using x$set) then inverse is set to NULL
    # Hence, check for is.null of inverse checks if inverse has not been computed or matrix has changed

    if (!is.null(inv)) {    # Check if inverse is not NULL (and hence if matrix has not changed) 
        message("Getting inverse of matrix from cache")
        return(inv)
    }
    # Either inverse has not been computed or matrix has been changed (and hence inverse has been initialized to NULL)
    message("Calculating inverse of matrix")
    y <- x$get()            # Get original matrix
    inv <- solve(y)         # Assuming matrix is a square invertible matrix
    x$setinverse(inv)       # Set inverse
    inv                     # Return inverse
}