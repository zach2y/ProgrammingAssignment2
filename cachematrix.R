## The first function creates a special object that stores a matrix and the inverse of the matrix.
## The second one checks and returns a cache inverse matrix if available. If not it calculates the inverse.

## This function creates a special "matrix" object that can cache its inverse.
## It is a list that contains a function to set the value of the matrix, get the value of the matrix, 
## set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## It first checks if the matrix inverse has already been calculated, if it was then 
## it gets the inverse and skip computation.
## If not, it then computes the inverse matrix and sets the value of the inverse using the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
