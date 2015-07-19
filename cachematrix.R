## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix is a construction method of "special" matrix objects

makeCacheMatrix <- function(x = matrix()) {
  ## First field of the makeCacheMatrix enviroment 
  matr<-x
  ##Second field inverse matrix
  inver<-NULL
  ##Flag which stores, if the object matrix was changed after inputting the inverse matrix
  change=FALSE
  ##Flag which stores if the object has cached the inverse matrix
  cached=FALSE
  
#Getter and setter methods for the fields
#Get the matrix
  getMatr<-function(){
    matr
  }

##Set matrix and change the chnage flag in we enter a new matrix after
##placing the inverse matrix in the object
  setMatr<-function(y=matrix()){
    matr<<-y
    if(isCached()){
      setChanged(TRUE)
    }
  }
  ##Get inverse matrix
  getInv<-function(){
    inver
  }
  ##Set inverse matrix if and only if isn't a empty matrix or na 
  setInv<-function(y=matrix()){
    if((dim(y)==dim(getMatr()))&&(!is.na(y)||!is.null(y))){
    inver<<-y
    setChanged(FALSE)
    setCached(TRUE)
    }else{
      print("The entered matrix doesn't seem to be right...")
      setCached(FALSE)
    }
  }
  ##Get change flag
  isChanged<-function(){
    change
  }
  #Set change flag
  setChanged<-function(y=logical()){
   change<<-y
  }
  #Get cached flag
  isCached<-function(){
    cached
  }
  #Set cached flag
  setCached<-function(a=logical()){
    cached<<-a
  }
  
  #Returned object of the constructor method "makeCacheMatrix"
  list(setMatr=setMatr,getMatr=getMatr,getInv=getInv,setInv=setInv,isChanged=isChanged,isCached=isCached)
}


## Write a short comment describing this function
##cacheSolve is a simple method which calculates the inverse matrix of the special matrix

cacheSolve <- function(x, ...) {
  z<-NULL
  #print("out")
        ## Return a matrix that is the inverse of 'x'
  
  ##  Condition of special matrix:
    ## isCached? ||  isTheMatrixChanged?||What to do?
    ##-----------------------------------------------
    ##  FALSE    ||       FALSE       -->Calculate the inverse matrix
    ##   TRUE    ||       FALSE       -->Retrun the cached inverse matrix
    ##  FALSE    ||       TRUE        -->Can't happen,because the change flag of the special matrix vector
                                     ##changes states if and only if the matrix is changed after an inverse matrix
                                     ##is already been submitted in the object
    ##   TRUE    ||       TRUE        -->Calculate the inverse matrix,again
  
  ##The below if-else if it is an implamantation of the above truth table
  if((x$isCached())&&(!(x$isChanged()))){
    #print("a")
    z<-x$getCached()
    print("The matrix object has cached the inverted array, to get it please call the method x$getInv()")
  }else if((x$isCached())&&((x$isChanged()))||(!x$isCached())&&((!x$isChanged()))){
    #print("c")
    z<-solve(x$getMatr())
    x$setInv(z)
  }
  z
}
