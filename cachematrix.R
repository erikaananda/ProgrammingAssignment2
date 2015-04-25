## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## setting so that 'getMatrix' doesn't error out on first call
  
  set <- function(y) {
    ## outside call uses 'set' to create initial matrix. 
    x <<- y
    ## saving the matrix outside of this environment
    m <<- NULL
    ## resetting m in the calling env.
  }
  get <- function() x
  ## if the matrix has been saved outside of this env, 'get' will find it
  
  setMatrix <- function(matrix) m <<- matrix
  ## saves a new matrix in the external env.
  
  getMatrix <- function() m
  ## if it exists, returns the external matrix. if not, returns NULL
  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
  ## function returns a list of functions.
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## x is a list of functions created through 'makeCacheMatrix'
  m <- x$getMatrix()
  ## if the matrix has been set through 'makeCacheMatrix', this returns it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if there is no saved matrix, get it from the calling function, 
  ## and run solve for the inverse, saving the results to m
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
