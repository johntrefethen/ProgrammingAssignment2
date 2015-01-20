## This file contains two functions - makeCacheMatrix() and cacheSolve()
## makeCacheMatrix - takes a matrix, stores it, and returns functions that can 
## get or set the matrix, as well as get or set the inverse of the matrix
## cacheSolve - take a list in the form of the return value from makeCacheMatrix()
## then determines the inverse by looking in cache first, and if not in cache, 
## computing the value using solve() function and returning it.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # create variable to hold the inverse of the matrix passed in
        inv <- NULL
        
        # check our environments
        print(environment())
        evn <- environment()
        print(parent.env(evn))
        
        # set the value of the matrix, and set variable holding
        # inverse of the matrix to NULL (essentially, reset the inv)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # return the the matrix we're using
        get <- function() x

        # set the inverse of the matrix
        setinverse <- function(inverse) {
                inv <<- inverse
                print("inside set inverse function")
        }
        # get the inverse of the matrix
        getinverse <- function() inv
        
        # get the environment we're using
        getevn<- function() environment()
        
        # return list of matrix operations we can use
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
             getevn = getevn)
}


## Function cacheSolve takes a list input variable and determines 
## the inverse of the matrix included in the list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # create a variable to hold the matrix
        inv <- x$getinverse()

        #print(inv)
        # check if it is NOT NULL
        # if NOT NULL, then return the result of solve(x)
        if (!is.null(inv)) {
                print("inside if")
                print("getting cached data")
                
                # Need a check here to confirm matrix has not changed
                
                return(inv)
        }
        
        # if we get here, matrix inverse was NULL
        # set the inverse using set function
        # then get it inverse using get
        print("after if loop")
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
