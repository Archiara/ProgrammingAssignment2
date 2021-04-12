##Input comments that gives the entire description of
##the functions do

##The functions that is encoded is to fulfill the coursera of DataScience:R Programming
##Week3 assignment in Github made April 12,2021
##Github-Archiara

## The function that is produced creates a special "matrix" object that can cache O.
##Creating a short a comment to the
makeCacheMatrix <- function(x = matrix()) {         ## Determines the argument with default mode of "matrix"      
            o <- NULL                               ## Put o as NULL; will hold value of matrix mean
            set <- function(y) {                    ## Determine the set function and assign it to new
                    x <<- y                         ## assigns value of o in parent environment
                    o <<- NULL                      ## if new matrix is found or determined, reset o to NUL
            }
            get <- function() x      ##defines the function to get mean
            setO <- function(mean) o <<- mean           ##Assigns the value of mean   
            getO <- function() o                ##function to set mean       
            list(set = set, get = get, setO = setO, getO = getO)                ##This is needed to 
            }                                                                   ##Refer the function with $ operator

##Input comments that gives the description of the
##function that returns the mean to matrix.
##Function calculates the mean of the special "matrix" returned by makeCacheMatrix that is seen above.
##If the mean has been calculated where the matrix did not change,
##then cachesolve will regain the mean.

cacheSolve <- function(x,...){
            ## Return the matrix which is the mean of 'x'
            o <- x$getmean()
            if(!is.null(o)){
            message("getting cached data!")
            return(o)
            }
            mtrx<- x$get()
            o <- solve(mtrx, ...)
            x$setO(o)
            o
            }

