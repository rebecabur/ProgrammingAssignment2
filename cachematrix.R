## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix receives a matrix and it has "setter" and "getter" methods
## if the inverse has not been calculated yet it's value will be null
## otherwise, it will have it's value storaged in the "i" variable

makeCacheMatrix <- function(x = matrix()) {# input x will be a matrix
  
  i <- NULL    #  i will be our 'inverse' and it's reset to NULL every 
  #    time makeCacheMatrix is called
  
  #  note these next three functions are not run when makeCacheMatrix is called.
  #   instead, they will be used by cachesolve() to get values for x or for
  #   i (inverse) and for setting the inverse.  These are usually called object 'methods'
  
  set <- function(y) {    # takes an input matrix
    x <<- y         # saves the input matrix 
    i <<- NULL      # resets the inverse to NULL, basically what happens when a new object is generated.
  }
  
  get <- function() { x }   # this function returns the value of the original vector
  
  setinverse <- function(inverse)  { i <<- inverse }
  # this is called by cachesolve() during the first cachesolve()
  #  access and it will store the value using superassignment
  
  getinverse <- function() { i } # this will return the cached value to cachesolve() on
  #  subsequent accesses
  
  list(set =set,                 #  OK, this is accessed each time makeCacheMatrix() is called,  
       get = get,                #   that is, each time we make a new object.  This is a list of
       setinverse = setinverse,  #   the internal functions ('methods') so a calling function 
       getinverse = getinverse)  #   knows how to access those methods. 
}


## cacheSolve receives as an argument an object created by the makeCacheMatrix function
## it returns the inverse of the matrix using the "solve" function
## if it has been calculated before, it returns the cached matrix.
## otherwise it calculates again. This helps to salve processing time.

cacheSolve <- function(x, ...) {   # the input x is an object created by makeCacheMatrix
  i <- x$getinverse()              # accesses the object 'x' and gets the inverse
  if(!is.null(i)) {                # if inverse was already cached (not NULL) ...
    
    message("getting cached data")  # ... send this message to the console
    return(i)                       # ... and return the inverse ... "return" ends 
    #   the function cachesolve(), note
  }
  data <- x$get()        # we reach this code only if x$getinverse() returned NULL
  i <- solve(data, ...)  # if i was NULL then we have to calculate the inverse
  x$setinverse(i)        # store the calculated inverse in x (see setinversen() in makeCacheMatrix
  i                      # return the inverse to the code that called this function
}
