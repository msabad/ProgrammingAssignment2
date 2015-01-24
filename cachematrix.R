## The following function creates a matrix and can calculate
##the inverse of the matrix and put it in cashe.

makeCacheMatrix <- function(x = matrix()) {
  ## creates a variable space named "matr" which will later hold a matrix object  
  matr <- NULL
  ## defines the set function, a function of y  
  set <- function(y) {
    ## designates x as a variable outside the function's environment.
    x <<- y
    ## designates a matrix variable space also outside the function's environment
    ## and sets its value to NULL; it will be replaced later with a matrix calculated
    ## using the solve function in R
    matr <<- NULL
  }
  ## defines a function of x outside the environment
  get <- function() x
  ## runs the solve function in R to calculate the inverse of matr
  set_matrix <- function(solve) matr <<- solve
  ## retrieves the function from outside the environment
  get_matrix <- function() matr
  ## defines a list of sub-functions to reset the variables set (the input matrix),
  ## get (the input matrix), set_matrix (the inverse) and get_matrix (the inverse)
  list(set=set, get=get,
       
       set_matrix=set_matrix,
       
       get_matrix=get_matrix)
  
}


## This function checks to see if there is already a matrix stored in cache.
## If there is one already stored, it returns that stored value, if not, it calculates
## the inverse of matr (the input matrix) and returns that newly calculated inverse.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  ## populates the matrix variable "matr" from the cache  
  matr <- x$get_matrix()
  ## checks if there is already a matrix in the matr variable space  
  if(!is.null(matr)){
    ## lets you know that there was a cached matrix which is being retrieved    
    message("getting cached matrix")
    ## sends the matrix to from the cache to output    
    return(matr)
    
  }
  ## gets the input matrix  
  matrix <- x$get()
  ## calculates the inverse of the input matrix  
  matr <- solve(matrix, ...)
  ## sets the value in matr to the newly calculated inverse matrix  
  x$set_matrix(matr)
  ## sends the newly calculated inverse to output  
  matr
 
}
