##Two Functions makeCacheMatrix and makeCacheMatrix
##makeCacheMatrix contains set,get,setinverse,getinverse
makeCacheMatrix <- function(x = matrix()) {
            o <- NULL
            set <- function(y) {
                    x <<- y
                    o <<- NULL
            }
            get <- function() x      #funtion to get matrix
            setmean <- function(mean) o <<- mean
            getmean <- function() o
                         inverse<-ginv(o)
				 inver%*%o
   				 }
            list(set = set, get = get, setinverse = setinverse, getInverse = getInverse)
            }

##function that returns the inverse to matrix. If
##inverse had been calculated. If so,gets the outcome and skips
##ccalculation. If not, it calculates the inverse, sets calue in cache via
##setinverse function

cacheSolve <- function(x,...){
            o <- x$getInverse()
            if(!is.null(o)){
            message("geting cached data!")
            return(o)
            }
            mat<- x$get()
            o <- solve(matrix, ...)
            x$setO(o)
            o
            }

