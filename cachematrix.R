## Put comments here that give an overall description of what your
## functions do
## These functions are designed to create a Matrix object which caches the Inverse
## of the Matrix for resuse with out recomputing the Inverse


## Write a short comment describing this function
## This function creates a list for the Matrix which contains member functions for
## 1 - Seting the value of the matrix
## 2 - Returning the matrix
## 3 - Computing the Inverse
## 4 - Returning the Inverse

makeCacheMatrix <- function(x = matrix()) {
        # Set the inverse to Null to begin  
        inverse <- NULL
        # The Set function will set the matrix within the CacheMatrix object to 
        # the matrix but it will not compute the inverse
        set <- function(y) {
          y <<- x
          inverse <<- NULL
        }
        # The Get function returns the matrix
        get <- function() x
        # The setInverse function sets the value to computed
        # This works because the super assignment operator <<- will go up in 
        # scope to the inervse variable defined by the makeCacheMatrix 
        # environment.  It exploits the lexical scoping rules of R.
        setInverse <- function(inverseComputed) inverse <<- inverseComputed
        # The getInverse function returns the current value of the Inverse
        getInverse <- function() inverse
        # Finally we return the list with the functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
      }


## Write a short comment describing this function
## This function returns the inverse of the matrix, but only calls solve(X) function 
## if the inervse has not been calculated before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        # if the inverse has been cached use it 
        if (!is.null(inverse)) {
                message("Using cached inverse")
                return(inverse)
        }
        #otherwise we have to compute it
        myMatrix <- x$get()
        inverseComputed <- solve(myMatrix)
        x$setInverse(inverseComputed)
        # and then return it
        inverseComputed
}
