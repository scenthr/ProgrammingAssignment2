## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix - takes a matrix parameter, has 4 subfunctions which have 
## different properties. All the subfunctions are presented through a list. 
makeCacheMatrix <- function(x = matrix()) {
## Each time the function is called the local matrix m is nullified        
        m <- NULL
        
## set function sets the new matrix values and sets m to NULL        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
## get function just returns x
        get <- function() x
## setInverse function calculates the inverts of the matrix and stores it in m
        setinverse <- function(solve) m <<- solve
## getInverse function retrieves the value from m        
        getinverse <- function() m
## this call here returns the list of possible functions through a list, and 
## enables the call of subfunctions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

## cacheSolve function takes into account already loaded matrix m 
## (using makeCacheMatrix) function and first checks if the inverse of that
## matrix has already been calcualted and stored. Then if it is stored retrieves
## the value from it,  otherwise it calcualtes the inverse of the loaded matrix
## and stores it using the object created by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m        
        
}
