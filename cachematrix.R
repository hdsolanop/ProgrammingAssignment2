## Put comments here that give an overall description of what your
## functions do

##This function creates a special "list" object 
##that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse=setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix referenciated 
## with the "list" created with the above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
