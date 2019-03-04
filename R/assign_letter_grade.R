#' Assign letter grades
#'
#' Assign a letter grade based on the grade computed by the \code{compute_grade} function.
#'
#' @param x A \code{tbl_prof} object.
#' @param interval A numeric vector specifying interval for grades.
#' @param letter_grade A character vector of letter grades.
#' @return a \code{tbl_prof} object.
#' 
#' @importFrom dplyr bind_cols %>%
#' @export

assign_letter_grade <- function(x, interval = NULL, letter_grade = NULL){

	if(is.null(interval)) interval <- c(-Inf, 60, 70, 80, 90, Inf)
	if(is.null(letter_grade)) letter_grade <- c("F", "D", "C", "B", "A")

	total <- attr(x, "total")

	grades <- cut(total, interval, letter_grade, include.lowest = TRUE, right = FALSE)
	out <- dplyr::bind_cols(x, grade = grades)

	if(!is.null(attr(x, "total_dropped"))){
		total_dropped <- attr(x, "total_dropped")
		grades_dropped <- cut(total_dropped, interval, letter_grade, include.lowest = TRUE, right = FALSE)
		out <- dplyr::bind_cols(out, grade_dropped = grades_dropped)
	}

	out
}
