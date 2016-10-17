## R programing course: Assignment #2

## The following pair of functions will enable to user to cache the inverse of a matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## This "matrix" is a list containing a function to:
##      - "set" : set the value of the matrix
##      - "get" : get the value of the matrix
##      - "setinverse" : set the value of the inverse matrix
##      - "getinverse" : get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve ## compute the inverse matrix
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve:  This function calculates the inverse of the special "matrix" created with 
##              "makeCacheMatrix". It first checks to see if the inverse has already been
##              calculated and if so, it uses the "get" function to get the inverse from the 
##              cache and skips the computation. Otherwise, it calculates the inverse of the 
##              input matrix and sets the value of the inverse in the cache via the "setinverse"
##              function. 
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
