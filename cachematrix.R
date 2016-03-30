## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a "matrix", which is actually a list containing function to
## set - set the value of the matrix
## get - get the value of the matrix
## setinverse - set the value of the matrix, inversed to the original one
## getinverse - get the value of the matrix, inversed to the original one

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x<<-y
                inverse<<-NULL
        }
        get <- function() x
        setinverse <- function(solved_matrix) inverse <<- solved_matrix
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function calculate inverse matrix for the input one and returns if it is possible
## and showes a warning message if it is impossible
## before calculating function checks if the matrix had already been calculated, if so it gets the result
## from cache and skips computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        if (det(x$get())!=0) {
                inverse <- solve(x$get(),...)
                x$setinverse(inverse)
                inverse
                } else {
                        message("There is no inversed matrix")
                        x$setinverse("There is no inversed matrix!")
                }
}
