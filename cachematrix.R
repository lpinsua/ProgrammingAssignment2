## Johns Hopkins University
## Data Science Specialization
## R Programming
## Luis Pedro Insua

#### PROGRAMMING ASSIGMENT 2

## The following two functions can be used together to cache the inverse
## of a matrix.


## Build a set of functions and return it to a list called makeCacheMatrix
## It will set the values of a matrix, get the valus of a matrix, set the value
## for the matrix solve, and get the value for the matrix solve.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The next function solves the matrix used in the above function, but first
## it checks if the solution has already been calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}




## Testing the functions
aMatrix <-makeCacheMatrix(matrix(c(-3,5,1,0),2,2))
aMatrix$get()
aMatrix$getsolve()
aMatrix$set(matrix(c(2,2,3,2),2,2))
cacheSolve(aMatrix)
aMatrix$getsolve()
