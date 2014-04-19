## The 'cacheMatrix.R' contains two major functions that support caching of matrix
## for time-consuming computations.  Of particular interest is the computation of 
## several matrix inversions.
##
## The following are the functions:
##   1. makeCacheMatrix()
##   2. cacheSolve()
## Details of each function can be found below.
##
## Author: Ma. Rowena C. Solamo
##
## This is a partial requirements to the R Programming Course in coursera.org
## dated April 7 - May 5, 2014 Session under the guidance of Prof. Roger D. Peng
## Johns Hopkins Bloomberg School of Public.
##
## The course is part of the Data Science Specialization found in coursera.org.
##
## Reference: Google's R Style Guide (https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml)

## The 'makeCacheMatrix()' function creates a special list representing a matrix 
## that can cache its inverse.  To do that, the special list contains four functions
## that sets and gets the matrix and its inverse on a different environment using
## the '<<-' operation.
makeCacheMatrix <- function (theMatrix = matrix()){
     ## Arguments:
     ## 'theMatrix' is a matrix whose inverse will be computed and stored
     
     ## Returns:
     ## a special list containing the following elements that are functions:
     ##   1. setMatrix() which assigns the matrix to theMatrix stored on a different
     ##        environment
     ##   2. getMatrix() which returns the matrix
     ##   3. setInverseOfMatrix() which assigns the inverse of the matrix to theInverse
     ##        stored on a different environment
     ##   4. getInverseOfMatrix() which returns the inverse of the matrix
     
     theInverse <- NULL    # initialization
     
     ## The 'setMatrix()' function sets the matrix to theMatrix.
     setMatrix <- function(y) {
          ## Arguments:
          ## 'y' is a matrix assigned to theMatrix which is stored on a different
          ##        environment.
                    
          theMatrix <<- y
          theInverse <<- NULL  # assumes theInverse has not been computed or theMatrix has been changed
     }
     
     ## The 'getMatrix()' function returns theMatrix.
     getMatrix <- function () { 
          ## Returns:
          ## the matrix normally set during the call to the setMatrix() function
          theMatrix
     }          
     
     ## The 'setInverseOfMatrix()' function sets the inverse of theMatrix.
     setInverseOfMatrix <- function (inverse) {
          ## Arguments:
          ## 'inverse' is a matrix the represents the inverse of theMatrix
          theInverse <<- inverse
     }
     
     ## The 'getInverseOfMatrix()' functions returns theInverse of theMatrix.
     getInverseOfMatrix <- function () {
          ## Returns:
          ## the inverse of the matrix normally set during the call to the
          ##       setInverseOfMatrix() function.
          theInverse
     }
     
     ## This creates the special list containing the functions.
     list (setMatrix=setMatrix, getMatrix=getMatrix, setInverseOfMatrix=setInverseOfMatrix, 
           getInverseOfMatrix=getInverseOfMatrix)
}

## The 'cacheSolve()' function basically computes the inverse of the special matrix
## created by the makeCacheMatrix() function.  If the inverse has already been computed
## before and the matrix has not been changed, then, the function returns the inverse
## of the matrix from the cache. Otherwise, compute the inverse, store it in the
## special matrix, and returns the computed inverse.
cacheSolve <- function (x, ...) {
     ## Arguments:
     ## 'x" is the special matrix created using the makeCacheMatrix()
     
     ## Returns:
     ## the inverse of the matrix
     
     ## 1. Get the inverse of the matrix.
     inverse <- x$getInverseOfMatrix()
     
     ## 2.  If the inverse is not null, then, return the cached inverse
     if (!is.null(inverse)){
          message ("getting cached inverted matrix")
          return (inverse)
     }
     
     ## 3.  Otherwise, do the following:
     ##     3.1 Get the matrix.
     aMatrix <- x$getMatrix()
     ##     3.2 Compute the inverse of the matrix using solve() function.
     inverse <- solve(aMatrix, ...)
     ##     3.3 Set the inverse of the matrix that stores it in the matrix's cache
     x$setInverseOfMatrix(inverse)
     ##     3.4 Return the inverse of the matrix.
     inverse
}