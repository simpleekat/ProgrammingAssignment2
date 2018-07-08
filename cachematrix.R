## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#this function will store "inverse" which will be used to cache the inverse 
#of the matrix

#it needs to be able to reset/change an existing matrix, and also be able to
#call a function to retrieve this object's inverse (for caching)
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      getinverse <- function() inverse
      setinverse <- function(calcinv) inverse <<- calcinv
      list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)

}


## Write a short comment describing this function

#this function will get the inverse of the special matrix object
#if the inverse variable is not NULL, then it will pull the cached value
# if the inverse variable is NULL, then it will calculate and store the 
#inverse value into the special matrix object
cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("Inverse has already been cached")
            return(inverse)
      }
      newmat <- x$get()
      inverse <- solve(newmat)
      x$setinverse(inverse)
      inverse
        ## Return a matrix that is the inverse of 'x'
}
