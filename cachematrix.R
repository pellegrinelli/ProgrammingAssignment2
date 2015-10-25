
## These two functions calculate the inverse of a matrix and cache the result 
## so if the input remains the same it does not redo the calculation.
## if the input changes, it recalculates the inverse. 


## makeCacheMatrix
##
## input: a square invertible matrix
##
## This function initiates the cache variable of the inverse matrix, sets the
## matrixValues (income parameter) and the storedInverse (as null), and 
## sets a list of functions that can be called (set, get, setInverse and
## getInverse). 
## The values of matrixValues and storedInverse remains the same unless
## the functions set and setInverse are called  or the function makeCacheMatrix
## is initiated again. These two variables are global in this function.
##
## Return Values: the return value is a list of functions to be used: set, 
## get, setInverse and getInverse.


makeCacheMatrix <- function(matrixValues = matrix()) {

    # initiates the variable that contains the inverse matrix
    storedInverse <- NULL

    # sets the values of the income matrix and resets the inverse matrix to 
    ## null
    set <- function(newMatrixValues) {
        matrixValues <<- newMatrixValues
        storedInverse <<- NULL
    }
    
    ## returns the values of the income matrix
    get <- function() matrixValues
    
    ## sets the values of the inverse matrix
    setInverse <- function(inverse) storedInverse <<- inverse
    
    ## returns the values of the inverse matrix
    getInverse <- function() storedInverse

    ## return values: list of the functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## cacheSolve
##
## input: the output of the makeCacheMatrix function
##
##
## First, this function checks if the inverse o the input matrix has already
## been calculated. If so, it returns the cached inverse matrix. If not,
## it gets the input, calculate the inverse of the matrix and sets in the
## cache variable the inverse. And then return the inverse matrix.
##
## return values: the inverse matrix

cacheSolve <- function(matrixList, ...) {
    
    ## Return a matrix that is the inverse of 'matrixValues'
    inverse <- matrixList$getInverse()
    
    ## checks if the inverse has already been calculated
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- matrixList$get()
    
    ## if the inverse is not cached yet, calculate the inverse matrix
    inverse <- solve(data, ...)
    
    ## set the inverse matrix cache
    matrixList$setInverse(inverse)
    
    inverse
}
