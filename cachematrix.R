
  ## Put comments here that give an overall description of what your
  ## functions do
  
  ## Write a short comment describing this function
  ## This function creates the Cache Matrix or Special matrix for the matrix that 
  ## has been provided
  
  makeCacheMatrix <- function(x = matrix()) {
    ## intialize the Inverse matrix
    Inv <- NULL
    
    ## set the new matrix
    set <- function(y) {
      x <<- y
      Inv<<- NULL
    }
    
    ## get the matrix 
    get <- function() x
    
    ## set inverse matrix 
    setInverse <- function(InvMatrix) Inv <<- InvMatrix
    
    ## get inverse matrix 
    getInverse <- function() Inv
    
    ## return the list of functions for the special(cache) matrix 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
  }
  
  
  ## Write a short comment describing this function
  ## This function gets the inverse of the matrix provided.It checks 
  ##if the inverse of the matrix is already available in the cache 
  ## If yes, then use the same ; else calculate the inverse using the solve function
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Return a matrix that is the inverse of 'x'
    Inv <- x$getInverse()
    
    ## check if the Inverse matrix is available in the Cache
    if(!is.null(Inv)) 
    {
      message("getting cached data")
      ## return the value of cached inverse matrix
      return(Inv)
    }
    
    data <- x$get()
    
    Inv <- solve(data, ...)
    ## Set the inverse Matrix to the cache
    
    x$setInverse(Inv)
    
    ## return the value of Inverse Matrix
    Inv
  }