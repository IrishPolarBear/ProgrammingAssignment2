## These two functions will create a matrix and then return the inversion. It also assumes that the matrix is invertible

## This function creates the matrix and it has functions to return the inversion, the matrix itself, and if there have been
## changes to the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    change <- FALSE
    set <- function(y){
        x <<- y
        i <<- NULL
        change <<- TRUE
    }
    get <- function() x
    setinverse <- function(inv) {
        i <<- inv
        change <<- FALSE
    }
    getinverse <- function() i
    getchange <- function() change
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse,
         getchange = getchange)
}


## This function will check to see if there is an inversion and if there have been no changes then it will return the saved value
## otherwise it will calculate the new inverted value and save it in the object. It will return the inverted matrix regardless.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    change <- x$getchange()
    if(!is.null(i) & !change){
        message("getting cached data")
        return(i)
    }
    else{
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)  
    }
    i
}
