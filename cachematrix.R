## The `makeCacheMatrix` function allows you to store an argument (a matrix) in cache. 
##The `cacheSolve`function returns the inverse of a matrix stored in cache from 'makeCacheMatrix'.
##If the inverse was previously calculated then it returns the result from the cache. 
## Otherwise it calculates the inverse, stores it in the cache and returns it.
## First, the inverse of the matrix is set to `NULL`. Then the  output is 4 functions
## to get and set the values of the argument, either just the matrix or the inverse. 

makeCacheMatrix <- function (x = matrix()) {
        b <- NULL
        
        list( getMatrix = function () x
              , getInverse = function () b
              , setInverse = function (c) b <<- c
              , setMatrix = function (y) {
                      x <<- y
                      b <<- NULL
              }
        )
}

## The `cacheSolve` function takes a cacheable matrix as an input and retrieves
## its cached inverse. Since we set the inverse to NULL in the first step of 'makeCacheMatrix',
## the function can return the inverse from cache if the inverse is not `NULL` 
## Otherwise it gets the actual matrix, calculates its inverse (passing
## additional parameters silently) and caches the result before returning it.

cacheSolve <- function (x, ...) {
        b <- x$getInverse()
        
        if (!is.null(b)) return(b);
        
        data <- x$getMatrix()
        b <- solve(data, ...)
        x$setInverse(b)
        
        b
}


## test case from staff TA in forum (updated with my variable names)
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## amatrix$getMatrix()         # Returns original matrix
## cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
## amatrix$getInverse()  # Returns matrix inverse
## cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

## amatrix$setMatrix(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
## cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
## amatrix$getMatrix()         # Returns matrix
## amatrix$getInverse()  # Returns matrix inverse 