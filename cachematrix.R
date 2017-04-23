## makeCacheMatrix() is a function that contains 4 functions that will be used by
## cacheSolve() and they will be passed through a list

## cacheSolve() is a function that returns the inverse of a matrix by saving 
## computing time in case it has already been calcultated by no calculating it again

## Let's start

## makeCacheMatrix() is a function that returns a bunch of functions (4)
## Those functions will be used in cacheSolve()

makeCacheMatrix <- function(a = matrix()) {
  ## Arguments: 'a'--> It is a square invertible matrix
  
  ## Return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ## This list is used as the input to cacheSolve()
  
  ## 'calculated.inv' will be our flag and we initialy set it a NULL value
  calculated.inv = NULL
  
  ## Defining each function's behaviour
  set = function(y) {
    # use `<<-` to assign a value to an object in a different environment that
    # currently one
    a <<- y
    calculated.inv <<- NULL
  }
  get = function() a
  setinv = function(inverse) calculated.inv <<- inverse 
  getinv = function() calculated.inv
  
  ## Return a list with all the functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve() returns a matrix that is the inverse of the argument 'a'and saving 
## computing time in case it has already been calcultated by no calculating it again

cacheSolve <- function(a, ...) {
  ## This function returns a matrix that is the inverse of the argument 'a'
  
  ## Arguments: 'a'--> it is the output of makeCacheMatrix()
  
  ## Return --> Returns the inverse of the original matrix input to makeCacheMatrix()
  
  ## Calculation --> The function cacheSolve() will double-check if the inverse
  ## has already been calculated
  ## If so, skips calculation
  ## If not, calculates the inverse
  
  ## Once it is finished, the inverse's value will be storaged in cache
  
  calculated.inv = a$getinv()
  
  # Double-check if the inverse has already been calculated
  if (!is.null(calculated.inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(calculated.inv)
  }
  
  # If not, calculates the inverse 
  calculated.data = a$get()
  calculated.inv = solve(calculated.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  a$setinv(calculated.inv)
  
  return(calculated.inv)
}
