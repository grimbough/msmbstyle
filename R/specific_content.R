#' Indicate a "solution" within the book.
#' 
#' Functions to demark a 'solution' within an R Markdown document.
#' 
#' If your 'solution' is a single paragraph of text, you can use 
#'   \code{solution()} and provide the paragraph via the argument \code{text}.
#' 
#' If you need to provide more detail than a single paragraph,
#'   you can use the functions \code{solution_begin()} and \code{solution_end()} 
#'   to indicated the region of the document you wish to be included in the 
#'   question.  (This is similar to using \code{\\begin{}} and \code{\\end{}} 
#'   tags in a Latex document.).  These two functions must be used together,
#'   otherwise the resulting document will have unmatched HTML tags.
#'   
#' \code{solution_begin()} is a simple wrapper around \code{solution} with the
#'   \code{text} forced to be missing, and you can start a larger solution with
#'   either.  \code{solution_end()} should never be used on its own as it 
#'   closes bits of document structure that are expected to have been opened 
#'   already.  Doing so will have unexpected results on the final layout.
#' 
#' @param text \code{character}. Text that provides the body of the solution.
#' @param header \code{character}. Defines the text used to mark the start 
#'   of a 'solution'.  Set this if you want to refer to this as an 'Answer', 
#'   'Explanation', etc.
#'   '&#x25BA;' renders as a filled right pointing triangle in HTML.
#' @param toggle \code{logical}.  If set to \code{TRUE} the solution is 
#'   rendered in a collapsed format, with only the header defined in \code{header}
#'   visible and a '+' icon that will expand the solution when clicked. Setting
#'   to \code{FALSE} renders the complete solution with no option to collapse it.
#'   Default value is \code{TRUE}.
#' @param hidden \code{logical}.  If set to \code{TRUE} the solution will be
#'   included in the HTML, but it will not be visible i.e. \code{display: none;}.
#'   This allow solutions to be generated during rendering, but they are 
#'   completely hidden from the user in cases where the intention is to provide 
#'   them at a later date.  Default value is \code{FALSE}.
#' 
#' @seealso \code{\link{question}}
#' 
#' @export
solution <- function(text, header = "&#x25BA; Solution", toggle = TRUE, hidden = FALSE) {
    
    if (knitr::is_html_output()) {
        
        id <- generate_id2()
        
        id1 <- paste0("sol-start-", id)
        id2 <- paste0("sol-body-", id)
        
        part1 <- paste0(
               "<div class = \"solution\">",
               "<p class=\"solution-begin\">", header, 
               ifelse(toggle, 
                      sprintf("<span id='%s' class=\"fa fa-plus-square solution-icon clickable\" onclick=\"toggle_visibility('%s', '%s')\"></span>", id1, id2, id1), 
                      ""),
               "</p>",
               ifelse(toggle,
                      paste0("<div class=\"solution-body\" id = \"", id2,"\" style=\"display: none;\">"),
                      "<div class=\"solution-body\">")
        ) 
        
        if(hidden) {
            part1 <- stringr::str_replace(string = part1, 
                                          "\"solution\"",
                                          "\"solution\" style=\"display: none;\"" )
        }
        
        ## if there's no text then don't close the HTML div
        ## we expect this to be done later
        if(missing(text)) {
            part2 <- ""
        } else {
            part2 <- paste0("<p>", text, "</p>",
                "<p class=\"solution-end\">", 
                "<span class=\"fa fa-square-o solution-icon\">", "</p>",
                "</div></div>")
        }

        
        output <- structure(paste0(part1, part2), format = "HTML", class = "knitr_kable")
        
    }    # TODO
    #else if (knitr::is_latex_output()) {
    #    
    #}
    else {
        warning("solution() only works for HTML output", 
                call. = FALSE)
        text
    }
    return(output)
}

#' @rdname solution
#' @export 
solution_begin <- function(header = "&#x25BA; Solution", toggle = TRUE, hidden = FALSE) {
    solution(header = header, toggle = toggle, hidden = hidden)
}

#' @rdname solution
#' @export
solution_end <- function() {
    if (knitr::is_html_output()) {
        
        part2 <- paste0("<p class=\"solution-end\">", 
                        "<span class=\"fa fa-square-o solution-icon\">", "</p>",
                        "</div>",
                        "</div>") 
        output <- structure(part2, format = "HTML", class = "knitr_kable")
    }
    return(output)
}

#################################################
## questions ####################################
#################################################


#' Indicate a "question" within a book.
#' 
#' Functions to demark a 'question' within an R Markdown document.
#' 
#' If your question is simply a paragraph of text, you can use 
#'   \code{question()} and provide the paragraph via the argument \code{text}.
#' 
#' If you need to provide more detail than a single paragraph,
#'   you can use the functions \code{question_begin()} and \code{question_end()} 
#'   to indicated the region of the document you wish to be included in the 
#'   question.  (This is similar to using \code{\\begin{}} and \code{\\end{}} 
#'   tags in a Latex document.).  These two functions must be used together,
#'   otherwise the resulting document will have unmatched HTML tags.
#'   
#' \code{question_begin()} is a simple wrapper around \code{question} with the
#'   \code{text} forced to be missing, and you can start a larger question with
#'   either.  \code{question_end()} should never be used on its own as it 
#'   closes bits of document structure that are expected to have been opened 
#'   already.
#' 
#' @param text \code{character}. Text that provides the body of the question.
#'   Not providing anything to this argument is assumed to indicate that
#'   you wish to include something more complex than just text within the 
#'   question, and a corresponding call to \code{question_end} should be 
#'   used later in the document.
#' @param header \code{character}. Defines the text used to mark the start 
#'   of a 'question'.  Set this if you want to refer to this as an 'Exercise', 
#'   'Homework', etc.  
#'   '&#x25BA;' renders as a filled right pointing triangle in HTML.
#' @param label \code{character}.  Provides a label that can be referenced in
#'   rest of the document.  This must start with the text 'ques:' and should
#'   be referenced using the standard bookdown syntax e.g. for
#'   \code{label = 'ques:one'} use \code{\@ref(ques:one)} in the main text.
#' 
#' @examples 
#' msmbstyle::question(text = "This is a labelled question",
#'                     label = "ques:two", 
#'                     header = "Answer")
#' 
#' @seealso \code{\link{solution}}
#' 
#' @export
question <- function(text, header = "&#x25BA; Question", label = NULL) {
    
    if (knitr::is_html_output()) {
        
        label <- ifelse(is.null(label), "", label)
    
        part1 <- paste0(
            sprintf("<div class = 'question' id='%s'>", label),
            "<p class='question-begin'>", header, "</p>",
            "<div class='question-body'>")
        
        if(missing(text)) {
            part2 <- ""
        } else {
            part2 <- paste0("<p>", text, "</p>",
                "<p class='question-end'>", "<span class='fa fa-square-o solution-icon'>", "</p>",
                "</div></div>")
        }

        ## reuse the knitr_kable structure to ensure it is included in the document
        ## if we just return a character() it includes the quotes around the HTML
        output <- structure(paste0(part1, part2), format = "HTML", class = "knitr_kable")
            
    
    }# TODO
    #else if (knitr::is_latex_output()) {
    #    
    #}
    else {
        warning("question() only works for HTML output", 
                call. = FALSE)
        text
    }
    return(output)
}

#' @rdname question
#' @export 
question_begin <- function(header = "&#x25BA; Question", label = NULL) {
    question(text = "", header = header, label = label)
}

#' @rdname question
#' @export
question_end <- function() {
    
    if (knitr::is_html_output()) {
        part2 <- paste0("<p class=\"question-end\">", 
                        "<span class=\"fa fa-square-o solution-icon\">", "</p>",
                        "</div></div>") 
        output <- structure(part2, format = "HTML", class = "knitr_kable")
    }
    
    return(output)
}
