## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function (theMatrix = matrix()){
     ## 'x' is a matrix
     
     ## Creates a special list containing the following functions:
     ## - set the matrix
     ## - get/return the matrix
     ## - set the inverse of the matrix
     ## - get/return the inverse of the matrix
     
     theInverse <- NULL
     
     ## The function definition of setMatrix()
     setMatrix <- function(y) {
          ## 'y' is a matrix
          
          ## sets y, which is matrix, to x is placed in a different environment
          ## sets the value m in a different environment; this value
          ##     will be used to check if the inverse of the matrix has been computed
          
          theMatrix <<- y
          theInverse <<- NULL
     }
     
     ## The function definition of getMatrix()
     getMatrix <- function () { 
          ## return a matrix normally set during the call to
          ##     the set() function
          theMatrix
     }          
     
     ## The function definition of setInverseOfMatrix()
     setInverseOfMatrix <- function (inverse) {
          ## 'inverse' is a matrix the represents the inverse of the matrix
          theInverse <<- inverse
     }
     
     ## The function definition of getInverseOfMatrix()
     getInverseOfMatrix <- function () {
          ## returns the inverse matrix normally set during the call to the
          ##       setinverse() function
          theInverse
     }
     
     ## creates the special list
     list (setMatrix=setMatrix, getMatrix=getMatrix, setInverseOfMatrix=setInverseOfMatrix, 
           getInverseOfMatrix=getInverseOfMatrix)
}

## Write a short comment describing this function

cacheSolve <- function (x, ...) {
     ## 'x" is a special list created using the makeCacheMatrix()
     
     ## 1. Get the inverse of the matrix
     inverse <- x$getInverseOfMatrix()
     
     ## 2.  If the inverse is not null, then, return the cached inverse
     if (!is.null(inverse)){
          message ("getting cached inverted matrix")
          return (inverse)
     }
     
     ## 3.  Otherwise, do the following:
     ##     3.1 get the matrix
     aMatrix <- x$getMatrix()
     ##     3.2 compute the inverse of the matrix
     inverse <- solve(aMatrix, ...)
     ##     3.3 set the inverse of the matrix in the cache
     x$setInverseOfMatrix(inverse)
     ##     3.4 return the inverse of the matrix
     inverse
}