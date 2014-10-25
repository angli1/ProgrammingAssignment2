
## Matrix inversion is usually a very time-consuming computation. we can cache the inverse of a matrix, 
## and look it up in the cache when it is needed later. This file contains two functions which take advantage of 
## the scoping rules of R language, set, compute, and retrieve the inverse of a given matrix. 
## 
## examples of usage:
## matrix_test <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))   ## the given matrix is a 2x2 square matrix 
## matrix_test$get()     				## check the given matrix 
## cacheSove(matrix_test) 				## get the inverse of the given matrix																				
## matrix_test$set(matrix(rnorm(16), 4, 4)))    	## set a new 4x4 square matrix 
							## cacheSolve(matrix_test) 							## compute the inverse of new 3x4 matrix 
   

## The following function  "makeCacheMatrix",  creates a special "matrix" object, 
## which is really a list containing a function to do the following:
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse of the matrix
##     get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL					## "inv" is the variable used to hold the inverse of the matrix
	set <- function(y) {
		x <<- y					## the super-assignment operator, assigns value to x in the 
							## containing environment(i.e., global variable here) 
		inv <<- NULL				## similarly,  inv here is a global variable
	}
	get <- function() x								## retrieve the given matrix
	setinverse <- function(solve) inv <<- solve	## "solve" is a function used to compute the inverse of a given matrix
	getinverse <- function() inv			## retrieve the value of "inv", which is the inverse of a given matrix 
	list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)  
  
}


## The following function, cacheSolve, will do the following:
## 1. if the inverse of the given matrix is cached, retrieve it with a message "getting cached data"
## 2. otherwise, it computes the inverse of the given matrix, caches it and return the value  

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {				## if variable inv is not null, then return the cached value
		message("getting cached data")
		return(inv)
	}
	data <- x$get()					# assign the given matrix to variable "data"
	inv <- solve(data, ...)				## compute the inverse of the matrix using "solve" function
	x$setinverse(inv)
	inv
  
}


