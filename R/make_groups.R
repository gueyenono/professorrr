#################################################
# ~*~ FORM GROUPS FOR PROJECTS AND HOMEWORK ~*~ #
#################################################


# Packages and seed number ----------------------------------------------------------------

library(openxlsx)
library(dplyr)
library(rlang)

set.seed(123)

# Import the data ---------------------------------------------------------

class <- read.xlsx("groups/ECON-312-001-Spring19 Grades.xlsx")


# Custom function to form groups ------------------------------------------

# > Helper functions ----

add_to_random <- function(x, n, addition){
  changed_i <- sample(length(x), size = n, replace = FALSE)
  x[changed_i] <- x[changed_i] + addition
  x
}

substract_from_random <- function(x, n, substraction){
  changed_i <- sample(length(x), size = n, replace = FALSE)
  x[changed_i] <- x[changed_i] - substraction
  x
}

# > Actual function ----

data <- class
group_size <- 3
alt_group_size <- 4
group_id <- "group"

make_groups <- function(data, group_size, alt_group_size, group_id = NULL){
  
  # alt_group_size = c("smaller", "larger")
  
  if(!is.null(group_id)){
    
    id <- which(is.na(data[[rlang::quo_name(group_id)]])) # row id of students with no group
    left_behind <- length(id) %% group_size # number of students who are left behind given the group_size
    
    if(left_behind == 0){
      
      n_groups <- length(id) %/% group_size
      groups <- rep(seq_len(n_groups), each = group_size)
      
    } else {
      
      if(alt_group_size == "smaller"){
        
        case_when(
          
        )
        
      } else if(alt_group_size == "larger"){
        
        case_when(
          
        )
        
      } else {
        
        stop('The alt_group_size argument is not specified correctly. It should take one of the following values: "smaller" or "larger".')
        
      }
      
      # n_groups <- (length(id) %/% group_size) + 1
      # groups <- 
      #   case_when(
      #     alt_group_size == "one_smaller" ~ rep(seq_len(n_groups), c(rep(group_size, n_groups - 1), left_behind)),
      #     alt_group_size == "several_smaller" ~ rep(seq_len(n_groups - 1), 
      #                                               rep(group_size, n_groups - 1) %>% substract_from_random(left_behind, 1))
      #   )
    }
    
    x <- rep(seq_len(n_groups), c(rep(group_size, n_groups - 1), left_behind))
    table(x)
    y <- rep(seq_len(n_groups - 1), 
             rep(group_size, n_groups - 1) %>% substract_from_random(left_behind, 1))

    
    # case_when(
    #   alt_group_size == "one_smaller" & left_behind != 0 ~ 1,
    #   alt_group_size == "several_smaller" ~ 2,
    #   alt_group_size == "one_bigger" ~ 3,
    #   alt_group_size == "several_bigger" ~ 4
    # )
    
  }
}


x <- rnorm(100)
y <- 2*x
z <- rnorm(100, 6, 4)

lm(y ~ x + z)
