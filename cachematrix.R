## This function creates a list of functions to: set value of the matrix,
## get value of the matrix, set the inverse of the matrix, and get the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## This function either retrieves the cached inverse matrix of "x" or 
## computes the inverse and cahches that matrix. 

cacheSolve <- function(x, ...) {
        i<- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data<- x$get()
        i<- solve(data, ...)
        x$setinverse(i)
        i
                
}
