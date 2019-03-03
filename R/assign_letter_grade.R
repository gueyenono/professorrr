#' Assign letter grades
#'
#' Assign a letter grade based on the grade computed by the \code{compute_grade} function.
#'
#' @param x A \code{tbl_prof} object.
#' @param intervals A numeric vector specifying intervals for grades.
#' @param letter_grades A character vector of letter grades.
#' @return a \code{tbl_prof} object.
#' 
#' @export

assign_letter_grade <- function(x, intervals = NULL, letter_grades = NULL){

	if(is.null(intervals)) intervals <- c(-Inf, 60, 70, 80, 90, Inf)
	if(is.null(letter_grade)) letter_grade <- c("F", "D", "C", "B", "A")

	total <- attr(x, "total")

	grades <- cut(total, intervals, letter_grades, include.lowest = TRUE, right = FALSE)
	out <- dplyr::bind_cols(x, grades)

	if(!is.null(attr(x, "total_dropped"))){
		total_dropped <- attr(x, "total_dropped")
		grades_dropped <- cut(total_dropped, intervals, letter_grades, include.lowest = TRUE, right = FALSE)
		out <- dplyr::bind_cols(out, grades_dropped)
	}

	out
}
