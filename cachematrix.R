#' cachematrix.R
#' 
#' @description
#' Functions for creating a special "matrix" object capable of caching
#' its inverse as well as caching its inverse. This was implemented
#' for Programming Assignment 2 in the R Programming course on Coursera
#' offered from August 4-September 5, 2014. See 
#' https://class.coursera.org/rprog-006/ for details.
#' 
#' Example code demonstrating how to use the code is available at the end
#' of this file.
#' 
#' @docType package
#' @name cachematrix
#' @author Craig Struble <strubleca@@yahoo.com>
#' 
#' NB: This documentation is in roxygen2 format, but won't format as is.
#' NULLs for bare documentation have been removed for clarity.


#' Create a special "matrix" object capable of caching its inverse.
#' 
#' This function creates a special "matrix" object 
#' 
#' The object is internally a list containing four functions used to set and 
#' get data from an environment created when the function is called. The list 
#' components are
#'     set - set the matrix data for the object
#'     get - get the matrix data for the object
#'     setinverse - set the matrix inverse for the object
#'     getinverse - get the matrix inverse for the object
#'     
#' When set is called, any previously set inverse will be cleared (set to NULL).
#' 
#' @param data the initial matrix data, defaults to an empty matrix.
#' @return a list with four components that are setter and getter functions.
#' @examples
#' testMat <- makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2))
#' testMat$get()
#' testMat$getinverse()
#' testMat$set(matrix(c(2,3,3,4), nrow=2, ncol=2))
#' testMat$setinverse(solve(testMat$get()))
#' testMat$getinverse()
makeCacheMatrix <- function(data = matrix()) {
    inv <- NULL                # Variable storing the matrix inverse
    
    # Setter "method", which updates the value of this special "matrix" object.
    set <- function(newData) {
        data <<- newData
        inv <<- NULL
    }
    
    # Getter "method", which retrieves the value of this special "matrix"
    get <- function() {
        data
    }
    
    # Setter "method", which sets the inverse of this special "matrix"
    setinverse <- function(inverse) {
        inv <<- inverse
    }

    # Getter "method", which retrieves the caches inverse value of this
    # special "matrix"
    getinverse <- function() {
        inv
    }
    
    # This list is the internal representation of the special "matrix"
    # object. Scoping rules in R are used to produce an environment
    # from this function call, which the "methods" share for storing
    # values into something similar to member variables.
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#' Return the inverse of a special "matrix" object, computing if required.
#' 
#' This function uses "solve" to return the inverse of matrix data stored
#' in a special "matrix" object. The inverse is computed only if the
#' inverse has not been cached previously.
#' 
#' @param mat the special "matrix" object created using makeCacheMatrix
#' @param ... additional parameters passed to "solve"
#' @return the matrix inverse computed by "solve"
#' @examples
#' testMat <- makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2))
#' cacheSolve(testMat)
cacheSolve <- function(mat, ...) {
    ## Return a matrix that is the inverse of 'mat'
    inv <- mat$getinverse()
    if(!is.null(inv)) { # If this is not NULL, a cached value present.
        message("getting cached data")
        return(inv)
    }
    # Compute the inverse and store it.
    data <- mat$get()
    inv <- solve(data, ...)
    mat$setinverse(inv)
    inv
}

#' An example of usage.
#' @name cachematrix-examples
#' @examples
#' # Create the initial matrix
#' testMat <- makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2))
#' 
#' # Retrieve its data
#' testMat$get()
#' 
#' # Get the inverse. Currently NULL.
#' testMat$getinverse()
#' 
#' # Compute and cache the inverse.
#' cacheSolve(testMat)
#' 
#' # Get the inverse. Should now be the matrix inverse.
#' testMat$getinverse()
#' 
#' # Computing again uses the cached result.
#' cacheSolve(testMat)
#' 
#' # Set the data to a new matrix.
#' testMat$set(matrix(c(2,3,3,4), nrow=2, ncol=2))
#' 
#' # Look at the new data to make sure its updated.
#' testMat$get()
#' 
#' # Get the inverse. Will now be NULL.
#' testMat$getinverse()
#' 
#' # Compute and cache the inverse. A second call returns the cached result.
#' cacheSolve(testMat)
#' cacheSolve(testMat)