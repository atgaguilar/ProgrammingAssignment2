##Module 2 R Programming 
##Week 3 Assignment: Lexical Scoping 

## makeCacheMatrix is a function creates a special "matrix" that can cache its inverse


makeCacheMatrix <- function(x = matrix())
{ 
  inverse <- NULL
  
  set <- function(y){
  
  x<<-y
  inverse <<- NULL
  
  }
 
  get <- function() x
  setInverse <- function(newInverse) inverse<<- newInverse
  getInverse <-function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}
 


## cacheSolve is a function which computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
   inverse <- x$getInverse()
  if(!is.null(inverse)){
    
    message("getting cached data")
    
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse      
}
