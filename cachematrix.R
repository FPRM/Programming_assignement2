
##                                 This is the cache and solved R function
## how to use:
## set a new variable "here is the a varible" with the make Cache function, with :

##        a<-makeCacheMatrix()

## set the matrix content with:

##        a$set(matrix(1:4,2,2)) 

## you can also use coerce to use non sequential numeric sequence for the matrix e.g "a$set(matrix(c(1,2,0,4),2,2))"
## Use the cache solve function to show the solved cache matrix with:

## cacheSolve(a)

## it returns the cache solved matrix like
##[,1] [,2]
##[1,]  1.0 0.00
##[2,] -0.5 0.25

makeCacheMatrix <- function(x = matrix())                        ## define the cache function 
{
     m <- NULL                                                   ## create an undefined variable
     set <- function(y)                                          ## take a matrix "y" and set it as global
     {
          x <<- y
          m <<- NULL
     }
     get<-function() x                                           ## get the "set" matrix
     setmat<-function(solve) m<<- solve                          ## set the m variable with the solved matrix
     getmat<-function() m                                        ## get the "cache inverse
     list(set=set, get=get,setmat=setmat, getmat=getmat)         ## return the attribute list used

}
cacheSolve <- function(x=matrix(), ...)                          ## Solving the cached matrix function
{
     m<-x$getmat()                                               ##store the solved matrix in a "m" variable
     if(!is.null(m))                                             ## check if the "m"matrix isn't empty
     {
          message("getting cached data")                         ## display message if the solved matrix is found
          return(m)                                              ## takes the cached matrix
     }
     matrix <- x$get()                                           ## if no matrix cached, do it with the attribute"get" from the list                                                    
     m<-solve(matrix, ...)                                       ## solve the matrix and store it in a variable
     x$setmat(m)                                                 ## set the cached variable with the new solved matrix
     m                                                           ## display the solved matrix
}