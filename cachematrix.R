## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The makeCacheMatrix function will create an object that is going to store
##the matrix in m and its reverse in mReverse. We can then use the set method to set a matrix
##get method to get the matrix stored, setReverse to set the matrix reverse and getReverse to
##get the matrix reverse.

makeCacheMatrix <- function(m = matrix()) {
        mReverse<-NULL
        set<-function(y){
                mReverse<<-NULL
                m<<-y
        }
        get<-function(){
                return(m)
        }
        setReverse<-function(reverse){
                mReverse<<-reverse
        }
        getReverse<-function(){
                return(mReverse)
        }
        
        return(list(get=get, set=set, getReverse=getReverse, setReverse=setReverse))
}


## Write a short comment describing this function
##The cacheSolve function will receive a makeCacheMatrix object and then check if it
##already has calculated its reverse by calling the method getReverse(). In case the
##reverse has been calculated before, it will return the stored value. Otherwise, it
##will use the function solve passing as an argument the matrix by calling the method
##get. Afterwards, it sets the reverse value by calling the setReverse method and finally
##returns the matrix reverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mReverse<-x$getReverse()
        if(!is.null(mReverse)){
                message("Getting cached matrix reverse")
                return(mReverse)
        }
        mReverse<-solve(x$get())
        x$setReverse(mReverse)
        return(mReverse)
}
