#################################################
# ~*~ FORM GROUPS FOR PROJECTS AND HOMEWORK ~*~ #
#################################################


# Packages and seed number ----------------------------------------------------------------

library(openxlsx)
library(dplyr)
library(rlang)

set.seed(123)

# Import the data ---------------------------------------------------------

class <- read.xlsx("data/ECON-312-001-Spring19 Grades.xlsx")


# Custom function to form groups ------------------------------------------

.data <- class
group_size <- s <- 3
alt_group_size <- "larger" # alt_group_size = c("smaller", "larger")
groups_col <- "group"

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

identify_incomplete <- function(.data, groups_col, s){
	groups_col <- rlang::parse_expr(groups_col)
	.data %>%
		group_by(!!groups_col) %>%
		tally(name = "count") %>%
		filter(!is.na(!!groups_col), count < s) %>%
		mutate(needed = s - count) %>%
		setNames(c("groups_col", "current_size", "needed"))
}

complete_groups <- function(.data, groups_col, s){ # groups_col needs to be a character here
	
	incomplete_groups <- identify_incomplete(.data = .data, groups_col = groups_col, s = s)
	
	if(nrow(incomplete_groups) == 0){
		
		return(.data)
		
	} else {
		
		potential_new_members <- which(is.na(.data[[groups_col]]))
		
		new_members <- tibble(
			id = sample(potential_new_members, size = sum(incomplete_groups$needed), replace = FALSE),
			groups_col = rep(incomplete_groups$groups_col, incomplete_groups$needed)
		)
		
		.data[[groups_col]][new_members$id] <- new_members[["groups_col"]]
		
		return(.data)
	}
}

x <- complete_groups(.data = .data, groups_col = "group", s = 3)

table(.data$group, useNA = "always")
identify_incomplete(.data = .data, groups_col = "group", s = 3) %>% nrow()

# > Actual function ----

make_groups <- function(.data, group_size, alt_group_size, groups_col = NULL){
  
  s <- group_size # Just to keep it in line with the paper's notation
  
  if(!is.null(group_id)){
    
    no_group_ids <- which(is.na(.data[[rlang::quo_name(groups_col)]])) # row id of students with no group
    n <- length(no_group_ids) # number of people who need a group
    g <- n %/% s # number of appropriately sized groups
    r <- n %% s # number of people "left behind" when all groups are initially formed
		
    # Identify incomplete groups and "top them off"
    .data[[rlang::quo_name(groups_col)]]
    
    
    
    if(r > 0){
    	
    	case_when(
    		
    	)

    } else {
    	
    	.data[[rlang::quo_name(groups_col)]][sample(no_group_ids)] <- rep(seq_len(n), each = s)
    	
    } 
        
    # stop('The alt_group_size argument is not specified correctly. It should take one of the following values: "smaller" or "larger".')
        
      
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



