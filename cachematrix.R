## These two functions set up a "matrix" object and calculate its inverse
## If the inverse is already cached, then it need not be calculated again

## This function creates a special "matrix" object with getter and setter functions

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<-  NULL
      }
      get <- function() x
      setinv <- function(i) inv <<- i
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function computes inverse of the special "matrix, if not already computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inv <- x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
