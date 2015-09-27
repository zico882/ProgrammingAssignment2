## Coursera: R programming Assignment 2
## SCOPING RULES
## ===================================================================
## ============================== USAGE ==============================
## 1. Create matrix from random normal variables
##    > mymat = matrix(rnorm(16,2,1), nrow = 4, ncol = 4)
##
## 2. Create special matrix object using 'makeCacheMatrix'
##    > mymatSP = makeCacheMatrix(mymat)
##
## 3. Compute inverse of special matrix created in 2
##    > cacheSolve(mymatSP)

##====================== OR SIMPLY CALL LIKE THIS =====================
## cacheSolve(makeCacheMatrix(matrix(rnorm(16,2,1),nrow = 4, ncol = 4)))

## makeCacheMatrix takes a matrix and creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                          # Initialize value of inv (stores matrix inverse)
        set <- function(y) {
                x <<- y                      # Assign value y to object 'x' in environment of function
                inv <<- NULL                 # set inv to NULL if matrix 'x' changes 
        }
        get <- function() x                  # get matrix 'x'
        setInv <- function(solve) {
                inv <<- solve                # compute & save inverse of matrix 'x'
        }
        getInv <- function() inv             # return the inverse of matrix 'x' 
        
        list(set = set, get = get,           # function returns a list
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve computes the inverse of the special "matrix" returned by ‘makeCacheMatrix’
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()                     # get inverse of matrix 'x' (NULL if not computed)
        if(!is.null(inv)) {                   # check if inv of matrix exists 
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()                       # get matrix 'x' 
        inv <- solve(data, ...)               # solve inverse of matrix
        x$setInv(inv)                         # Saves the inverse of matrix 'x' (inv)
        inv                                   # Return matrix inv, the inverse of 'x'
}
