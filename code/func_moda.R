##===============================================
# functions for factors
##-----------------------------------------------
# Author : Cedric Bezy
# Date : ...
##===============================================

is.complete <- function(v){ sum(is.na(v))==0 }

##===============================================
# VALEURS MANQUANTES
##===============================================

nna <- function(x){ na.omit(x) }

Lnna <- function(v){ length(nna(v)) }


##===============================================
# MODALITES
##===============================================

vect_values <- function(v){
    v <- na.omit(v)
    if(is.factor(v)){ 
        moda <- levels(v)
    }else{ 
        moda <- sort(unique(v))
    }
    return(moda)
}

# nombre modalites
nb_values <- function(v){
    v <- na.omit(v)
    if(is.factor(v)){
        res <- nlevels(v)
    }else{
        res <- length(unique(v))
    }
    return(res)
}

