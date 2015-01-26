##-----------------------------------------------

## Programming assignment 2

## Cache results of matrix inversion operation

##  - makeCacheMatrix 

##    creates an enhance matrix that is 

##    capable of saving previously calculated

##    result of matrix inversion calculation

##  - cacheSolve

##    checks if invesion already exists and

##    if so, returns the inversion else it

##    calculates the matrix inversion

##-----------------------------------------------

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL

        set <- function(y) {

                x <<- y

                i <<- NULL

        }

        get <- function() x

        setInverse <- function(inverse) i <<- inverse

        getInverse <- function() i

        list(set = set, get = get,

             setInverse = setInverse,

             getInverse = getInverse)

}

cacheSolve <- function(x, ...) {

        i <- x$getInverse()

        if(!is.null(i)) {

                message("getting cached inverse")

                return(i)

        }

        data <- x$get()

        i <- solve(data, ...)

        x$setInverse(i)

        i

}