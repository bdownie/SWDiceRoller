
#library(shiny)
#library(tibble)
#library(rccmisc)
library(plumber)


#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

#* Return the roll of some dice
#* @param green The number of green dice
#* @param purple The number of purple dice
#* @post /rollSWDice
rollSWDice <- function(green,purple) {
    # Success, Advantage
    green_die <- list(c(0,0),c(1,0),c(1,0),c(2,0),c(0,1),c(0,1),c(1,1),c(0,2))
    # Failure, Threat
    purple_die <- list(c(0,0),c(-1,0),c(-2,0),c(0,-1),c(0,-1),c(0,-1),c(0,-2),c(-1,-1))
    
    green <- sample(1:8, green, replace=TRUE)
    purple <- sample(1:8, purple, replace=TRUE)
    
    green <- colSums(sapply(green,function(x) {
        green_die[[x]]
    }))
    purple <- colSums(sapply(purple,function(x) {
        purple_die[[x]]
    }))
    data.frame(Success=green[1] + purple[1],Advantage=green[2] + purple[2])
    #green2[1]
    #tibble(Success=green[1],Advantage=green[2],Failure=purple[1],Threat=purple[2])
    
}
