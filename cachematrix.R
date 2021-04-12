##Input comments that gives the overall description of
##the functions do

##Two Functions which is the makeCacheMatrix and makeCacheMatrix
##makeCacheMatrix that comprises the set,get,setmean,getmean

##Making a short a comment to the
makeCacheMatrix <- function(x = matrix()) {             
            o <- NULL
            set <- function(y) {
                    x <<- y
                    o <<- NULL
            }
            get <- function() x      ##defines the function to get mean
            setO <- function(mean) o <<- mean           ##Assigns the value of mean   
            getO <- function() o                ##function to set mean  
                      
            list(set = set, get = get, setO = setO, getO = getO)                ##This is needed to 
            }                                                                   ##Refer the function with $ operator

##Input comments that gives the overall description of the
##function that returns the mean to matrix. If
##mean had been calculated. If so,gets the outcome and skips
##calculation. If not, it calculates the mean, sets value in cache via
##setmean function

cacheSolve <- function(x,...){
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

