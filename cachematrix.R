## Here are two functions for calculating and storing in cache inverse
## of given matrix.


## makeCacheMatrix function creates special list that contains
## 4 functions for setting and getting value of matrix and 
## setting inverse and getting inverse of matrix.
## This list is used as an argument in second function - cacheSolve.



makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y) {    # function for setting value of matrix, if the matrix is new
                x<<-y         # than function will rewrite value of cached inverse to NULL
                s<<-NULL
        }
        get<-function() x     # function for getting value of matrix for inverse calculation
        setinverse<-function(inverse) s<<-inverse   # function for setting value of inverse to cache
        getinverse<-function() s  # function for getting value of inverse stored in cache
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## cacheSolve function takes matrix that is given by makeCacheMatrix
## function and calculates its inverse. If inverse has been already
## calculated and initial matrix has not been cahnged than cacheSolve
## function returns inverse of matrix from cache. If it's happen 
## message "getting data from cache" will be shown.


cacheSolve <- function(x, ...) {
        
        s<-x$getinverse()    # getting value of inverse from 1-st function
        if(!is.null(s)) {    # checing if s (inverse value) has a value from previous calculations
                message("getting data from cache")
                return(s)    # if value is in the cache and the condition is true it prints the message and stops execution of the function
        }
        data<-x$get()        # if s is NULL - getting martix from 1-st function
        s<-solve(data, ...)  # calculating inverse of the matrix
        x$setinverse(s)      # setting value of calculated inverse and storing it within the first function 
        s
        ## Return a matrix that is the inverse of 'x'
}
