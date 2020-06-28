
## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix takes in a matrix X and converts it into a matrix Object
# capable of storing functions, the original matrix, and its inverse

# cacheSolve takes in the matrix object created by makeCacheMatrix
# if the inverse has been already calculated, it uses that. Otherwise, it calc's the inverse


## Write a short comment describing this function

# makeCacheMatrix takes in a matrix X and converts it into a matrix Object

# This object has 4 functions and 2 floating variables. 
# Functions: set, get, setInverse, and getInverse
# x, the original Matrix passed in, and inverseMatrix, the inverse of X if calculated

# Set replaces the current matrix stored in the object with a new one passed in. 
# Get retrieves the current matrix stored in the object
# SetInverse defines the Inverse Matrix in the object as the matrix passed in.
# GetInverse retrieves the Inverse Matrix currently stored in the object.

# list allows the commands to be callable using the $ operator
makeCacheMatrix <- function(x = matrix())
{
  inverseMatrix <- NULL
  
  set <- function(y)
  {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function()
  {
    x
  }
  
  setInverse <- function(newInverse)
  {
    inverseMatrix <<- newInverse
  }
  
  getInverse <- function()
  {
    inverseMatrix
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

#cacheSolve takes in a matrixObject and either returns the cached Inverse Matrix stored within the object if available
#or calculates the InverseObject and stores it back in the object. 

cacheSolve <- function(x, ...) 
{
  inverseMatrix <- x$getInverse
  if(!is.null(inverseMatrix))
  {
    message("getting cached data")
    return(inverseMatrix)
  }
  matrix = x$get()
  inverseMatrix = solve(matrix,...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
