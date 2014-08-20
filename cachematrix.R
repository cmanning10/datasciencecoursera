## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Instantiate matrix x and its inverse i in the environment
        # makeCacheMatrix.  i is instantiated to NULL, to be modified by
        # function cacheSolve when inverse is calculated.
        #
        # get() returns x
        # setinverse(inverse) saves matrix inverse from parent/calling
        #    environment cacheSolve() into variable i.
        # getinverse() returns i, which is NULL until modified by cacheSolve().
        #
        # Returns a list with functions get(), setinverse(), and getinverse().
        # Does not explicitly return matrix x, but x exists as a variable in
        #    makeCacheMatrix()'s environment (to be returned by function get()).
        #
        # Note: Logic copied from makeVector provided at 
        #   https://github.com/rdpeng/ProgrammingAssignment2, except
        #   the analogue of function "set" is eliminated because it is not used.
        #
        i <- NULL
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the matrix object returned by makeCacheMatrix(). If 
##   the inverse has already been calculated (and the matrix has not changed), 
##   then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Check for existence of inverse i of matrix x object created by 
        #   function makeCacheMatrix().  If it exists, return it using function 
        #   getinverse() from function makeCacheMatrix().
        #   Otherwise, calculate inverse of x and place into variable i..
        #
        # Returns i, the matrix inverse of x.
        #
        # Note: Logic copied from makeVector provided at
        #   https://github.com/rdpeng/ProgrammingAssignment2, except
        #   the analogue of function "set" is eliminated because it is not used.
        #
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}