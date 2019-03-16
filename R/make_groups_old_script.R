library(googlesheets)
library(tidyverse)
library(openxlsx)
library(janitor)

set.seed(123)

# Authenticate access to my drive

# token <- gs_auth(cache = FALSE)
# write_rds(token, "google_token.rds")
gs_auth(token = "google_token.rds")


# Import data files

roster <- 
	read.xlsx("groups/ECON-312-001-Winter19 Grades.xlsx") %>%
	clean_names() %>%
	select(matches("name|email")) %>%
	mutate(email_address = tolower(email_address),
			 first_name = str_replace(first_name, "(^.*)\\s\\(.*\\)$", "\\1")
	)

friends <- gs_read(gs_key("1W3_V8rSurPeIEU5mk8Zyq6a_-0mwZn4DOe-GPot3jDc"))

initial_groups <- 
	friends %>%
	clean_names() %>%
	mutate(first_name = ifelse(is.na(first_name),
										name_given_in_class,
										first_name),
			 email_address = str_to_lower(email_address)
	) %>%
	filter(!str_detect(first_name, "[Bb]en")) %>%
	slice(-nrow(.)) %>%
	select(first_name, group)

merged <-
	left_join(roster, initial_groups, by = "first_name") %>%
	arrange(group)

# Identify incomplete groups and how many members they need

identify_incomplete <- function(data, group_id, group_size){
	group_id <- rlang::enquo(group_id)
	data %>%
		group_by(!!group_id) %>%
		tally() %>%
		filter(!is.na(!!group_id), n < group_size) %>%
		mutate(needed = group_size - n) %>%
		setNames(c("group_id", "current_size", "needed"))
}

# Randomly select people to add to these groups and add them to the merged file

complete_groups <- function(data, group_id, group_size){
	
	group_id <- rlang::enquo(group_id)
	
	incomplete_groups <- identify_incomplete(data = data, group_id = !!group_id, group_size = group_size)
	
	potential_new_members <- which(is.na(data[[rlang::quo_name(group_id)]]))
	
	new_members <- tibble(
		id = sample(potential_new_members, size = sum(incomplete_groups$needed), replace = FALSE),
		group_id = rep(incomplete_groups$group_id, incomplete_groups$needed)
	)
	
	data[new_members$id, rlang::quo_name(group_id)] <- new_members[["group_id"]]
	data
}

identify_incomplete(data = merged, group_id = group, group_size = 3)
merged_2 <- complete_groups(data = merged, group_id = group, group_size = 3)

group_size <- 3

id <- which(is.na(merged_2$group))
max_group_id <- max(unique(merged_2$group), na.rm = TRUE)
group_id <- rep(seq_len(length(id)/group_size) + max_group_id, each = group_size)

merged_2[sample(id), "group"] <- group_id

merged_2 %>%
	arrange(group) %>%
	write_csv("groups/monetary_economics_winter_19_groups.csv")


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

add_to_random(1:10, 3, 200)
substract_from_random(1:10, 3, 200)

archaic_make_groups <- function(data, group_id, group_size = )

	
	
make_groups <- function(data, group_size, alt_group_size, group_id = NULL){
	
	if(!is.null(group_id)){
		
		id <- which(is.na(data[[rlang::quo_name(group_id)]]))
		left_behind <- length(id) %% group_size
		n_groups <- if_else(left_behind == 0,
								  length(id) %/% group_size,
								  length(id) %/% group_size + 1)
		
		
		if(left_behind == 0){
			groups <- rep(seq_len(n_groups), each = group_size)
		} else {
			groups <- 
				case_when(
					alt_group_size == "one_smaller" ~ rep(seq_len(n_groups), c(rep(group_size, n_groups - 1), left_behind)),
					alt_group_size == "several_smaller" ~ rep(seq_len(n_groups - 1), 
																			rep(group_size, n_groups - 1) %>% remove_from_random(left_behind, 1))
				)
		}
		
		case_when(
			alt_group_size == "one_smaller" & left_behind != 0 ~ 1,
			alt_group_size == "several_smaller" ~ 2,
			alt_group_size == "one_bigger" ~ 3,
			alt_group_size == "several_bigger" ~ 4
		)
		
	}
}