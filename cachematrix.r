## Put comments here that give an overall description of what your
## functions do
#Purpose of these functions are to calculate the inverse matrix-assuming square matrix
#Next is to cache the inverse matrix for a future function in order to avoid recalculating
#if the inverse is not calculated then the second function will calculate the matrix
#lastly the end product is the inverse matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #set invMatrix variable to NULL
        invMatrix <- NULL
        set <- function(y){
          #set a cache for maxtrix and invMatrix
                  x <<- y
                  invMatrix <<- NULL
        }
        get <- function() x
        #setinv sets the value of the matrix
        #getinv gets the value of the matrix
        setinv <- function(inverse) invMatrix <<- inverse
        getinv <- function() invMatrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        invMatrix <- x$getinv()
        #if statement is checking if invMatrix has been calculated
        if(!is.null(invMatrix)) {
                  message("getting cached inverse matrix")
                  return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data, ...)
        x$setinv(invMatrix)
        return(invMatrix)
        
#Check our product with basic 2x2 and 3x3 matrices

firstTest <- matrix(1:4,2,2)
firstTest1 <- makeCacheMatrix(firstTest)
cacheSolve(firstTest1)

secondTest <- matrix(rnorm(9),3,3)
secondTest1 <- makeCacheMatrix(secondTest)
cacheSolve(secondTest1)

}
