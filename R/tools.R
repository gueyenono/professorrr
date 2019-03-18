##############################################
# ~*~ HELPER FUNCTIONS AND GENERAL TOOLS ~*~ #
##############################################

#' @import rlang
#' @importFrom dplyr group_by tally filter mutate
#' 

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

top_off_groups <- function(.data, groups_col, s){ # groups_col needs to be a character here
	
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