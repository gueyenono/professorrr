#' Make groups
#'
#' Make groups by specifying the population size, the group size and other
#' parameters.
#'
#' @param .data A 2-dimensional structure (e.g. data frame, tibble, data.table,
#'   ...) that contains information about the population to be assigned into
#'   groups.
#' @param group_size An integer specifying the desired group size.
#' @param alt_group_size A character value taking \code{"smaller"} or
#'   \code{"larger"} to specify whether smaller or larger groups should be
#'   formed in case of imperfect scenarios.
#' @param groups_col A character value specifying whether a column in
#'   \code{.data} already exist. This is useful if the data already has
#'   observations already assigned to groups groups. In this case, the
#'   observations with no group should have \code{NA}.
#'   
#' @importFrom dplyr case_when
#' @export

make_groups <- function(.data, group_size, alt_group_size = "larger", groups_col = NULL){ # add max_difference = 1 argument
	
	# stop('The alt_group_size argument is not specified correctly. It should take one of the following values: "smaller" or "larger".')
	
	
	s <- group_size # Just to keep it in line with the paper's notation
	
	if(is.null(groups_col)){
		
		.data_2 <- .data
		
		no_group_ids <- which(is.na(.data[[groups_col]])) # row id of students with no group
		n <- length(no_group_ids) # number of people who need a group
		g <- n %/% s # number of appropriately sized groups
		r <- n %% s # number of people "left behind" when all groups are initially formed
		q <- r %/% g
		v <- r %% g
		group_index_start <- 1
		
	} else {
		
		# Identify incomplete groups and "top them off" if there are any
		.data_2 <- top_off_groups(.data = .data, groups_col = "group", s = 3)
		
		no_group_ids <- which(is.na(.data_2[[groups_col]])) # row id of students with no group
		n <- length(no_group_ids) # number of people who need a group
		g <- n %/% s # number of appropriately sized groups
		r <- n %% s # number of people "left behind" when all groups are initially formed
		q <- r %/% g
		v <- r %% g
		group_index_start <- max(.data_2[[groups_col]], na.rm = TRUE) + 1
	}
	
	if(r > 0 & g == 1) stop("Change the alt_group_size to smaller")
	
	if(r == 0){
		group <- rep(seq_len(g) + group_index_start, times = s)
	} else if(r > 0 & alt_group_size == "larger" & v == 0){
		group <- rep(seq_len(g) + group_index_start, times = s + q)  # method 1 / case 1
	} else if(r > 0 & alt_group_size == "larger" & r < g){
		group <- rep(seq_len(g) + group_index_start, # method 1 / case 2
			 times = c(rep(s + 1, r), rep(s, g - r)))
	} else if(r > g & alt_group_size == "larger" & v > 0){
		group <- rep(seq_len(g) + group_index_start, # method 1 / case 3
						 times = c(rep(s + q, g - v), rep(s + q + 1, v)))
	}
	
	
	# case_when(
	# 	r == 0                                       ~ rep(seq_len(g) + group_index_start,
	# 																		times = s),
	# 	
	# 	# r > 0 & s - r <= max_difference            ~ rep(seq_len(g) + group_index_start,
	# 	# 																	times = c(rep(s, g - 1), rep(r, 1))),
	# 	
	# 	r > 0 & alt_group_size == "larger" & v == 0  ~ rep(seq_len(g) + group_index_start,
	# 																		times = s + q),  # method 1 / case 1
	# 	
	# 	r > 0 & alt_group_size == "larger" & r < g   ~ rep(seq_len(g) + group_index_start, # method 1 / case 2
	# 																		times = c(rep(s + 1, r), rep(s, g - r))),
	# 	
	# 	r > g & alt_group_size == "larger" & v > 0   ~ rep(seq_len(g) + group_index_start, # method 1 / case 3
	# 																		times = c(rep(s + q, g - v), rep(s + q + 1, v)))
	# ) -> group
	
	.data_2[[groups_col]][sample(no_group_ids)] <- group
	
	.data_2
}