## Coursera: R programming Assignment 2
## SCOPING RULES
## =====================================

## makeCacheMatrix takes a matrix and creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL                 # Assign NULL to inv object
        }
        get <- function() x                  #  get matrix x
        setInv <- function(solve) {
                inv <<- solve                # solve for matrix inverse
        }
        getInv <- function() inv             # get solved matrix inverse
        
        list(set = set, get = get,           # create list from function
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve computes the inverse of the special "matrix" returned by ‘makeCacheMatrix’
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()                     # get inv matrix (NULL if not calculated)
        if(!is.null(inv)) {                   # check if inv of matrix exists
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()                       # get matrix 'x' 
        inv <- solve(data, ...)               # solve inverse of matrix
        x$setInv(inv)                         # Saves the inverse of matrix 'x' (inv)
        inv                                   # Returns a matrix inv, the inverse of 'x'
}
