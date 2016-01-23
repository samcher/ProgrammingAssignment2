## This function calculates the inverse of a matrix.
## The inverse is cached the first time and subsequent requests are returned from cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #solve
  #cache variable
  inversem <- NULL
  set <- function(y) {
    x <<- y
  }
  

  # return the original martix passed in
  get <- function() x
  setinverse <- function(inversematrix) inversem <<- inversematrix
  
  ## return the inverse. If already in cache return the cache
  getinverse <- function(){
    
    if(!is.null(inversem)) {
      message("getting cached data")
      return(inversem)
    }
    
    #uncomment this to see a delay of 5 seconds first time.
    #Sys.sleep(5)
    
    inversem <<- solve(x)
    #inversem
  } 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function prints the inverse of the matrix passed in


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## usage: m1 <- cacheSolve(makeCacheMatrix(), matrix(c(2, 4, 3, 1), nrow=2, ncol=2))
        ## the inverse of the matrix is printed
        ##      [,1] [,2]
        ## [1,] -0.1  0.3
        ## [2,]  0.4 -0.2
        ## to see the original matrix
        ## m1$get()
        ## to get the inverse
        ## m1$getinverse() the value is from the cache. not reclaculated
        ## more matrices can be created as:
        ## m2 <- cacheSolve(makeCacheMatrix(), matrix(c(2, 3, 4, 1), nrow=2, ncol=2))
        ## similar operations as shown above can done on m2
  
  x$set(...)
  inverse <- x$getinverse()
  print(inverse)
  x 
  
}
