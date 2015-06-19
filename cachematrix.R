## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inversemat <- NULL
  ## delcare another function set where the value will be cached 
 
  set <- function(y) {
    x <<- y
    ## change the value of inverse of the matrix in case the matrix was changed.
   inversemat <<- NULL
  }
  ## get the value of inverse
  get <- function() return(x);
  setinverse <- function(inv) inversemat<<- inv;
  getinverse <- function() return(inversemat);
  return(list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse))
}

#The following function calculates the inverse of matrix returned by makeCacheMatrix
#First checks to see if the inverse has already been calculated. 
#If so, it gets the inverse matrix from the cache and skips the computation. 
#Otherwise it computes the inverse of a square matrix  with the solve function

cacheSolve <- function(x, ...) 
  {
       
  inv <- x$getinverse()
  
  # if the inverse exist it gets it
  if(!is.null(inv)) 
    {
    message("retrieving the cached data")
    return(inv)
  }
  #if the inverse matrix does not exist , calculate the matrix and iverse it
  data <- x$get()
  #computing the inverse of a square matrix  with the solve function
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)

  
}
