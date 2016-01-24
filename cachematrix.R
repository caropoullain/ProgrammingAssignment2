##Write the following functions:

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.

##------------------------------------------------------------------------------------------

## This function creates a special object that can stores a matrix and caches its reverse. 
## It returns a list of four functions to set and get the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## define function to set value of the matrix and clear old inverse from cache
inv <- NULL
set <- function(y){
  ## set value
  x <<- y
  ## clear cache
  inv <<- NULL
}
## define function to get value of the matrix
get <- function() x
## define function to set inverse of the matrix. This is only used when there is no inverse in cache
setInverse <- function(inverse) inv <<- inverse
## define the function to get the inverse
getInverse <- function() inv

# return a list with the functions
list(set=set,
     get=get,
     setInverse=setInverse,
     getInverse=getInverse)
}


## This function cimputes the inverse of the above matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() ## this gets the cached value of the inverse
  if(!is.null(inv))   {  ## if there's already an inverse in the cache, just return it
    message("getting cached data")
    return(inv)
  }
## if the cache is empty, calculate the inverse with
  data <- x$get()    ## get value of matrix
  inv <- solve(data) ## calculate inverse via solve()
  x$setInverse(inv)  ## sets value of inverse via setInverse() and caches result
  inv                ## return inverse
}

## To test the function

my_matrix <- makeCacheMatrix(matrix(c(2,2,3,2), 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()
