
#' @export
solution_end <- function() {
    if (knitr::is_html_output()) {
        
        part2 <- paste0("<p class=\"solution-end\"", "\"style=\"display: none;\">", 
                        "<span class=\"fa fa-square-o solution-icon\">", "</p>",
                        "</div>",
                        "</div>") 
        output <- structure(part2, format = "HTML", class = "knitr_kable")
    }
    return(output)
}

#' @export 
solution_begin <- function(begin = "&#x25BA; Solution", toggle = TRUE) {
    solution(text = "", begin = begin, toggle = toggle)
}

#' @export
solution <- function(text = "", begin = "&#x25BA; Solution", toggle = TRUE) {
    
    if (knitr::is_html_output()) {
        
        #ids <- stringi::stri_rand_strings(n = 2, length = 8)
        id <- generate_id2()
        
        id1 <- paste0("sol-start-", id)
        id2 <- paste0("sol-body-", id)
        
        part1 <- paste0(
               "<div class = \"solution\">",
               "<p class=\"solution-begin\">", begin, 
               ifelse(toggle, 
                      sprintf("<span id='%s' class=\"fa fa-plus-square solution-icon clickable\" onclick=\"toggle_visibility('%s', '%s')\"></span>", id1, id2, id1), 
                      ""),
               "</p>",
               "<div class=\"solution-body\" id = \"", id2,"\" style=\"display: none;\">"
        )
        if(nchar(text) == 0) {
            part2 <- ""
        } else {
            part2 <- paste0("<p>", text, "</p>",
                "<p class=\"solution-end\" style=\"display: none;\">", 
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

#################################################
## questions ####################################
#################################################

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

#' @export 
question_begin <- function(begin = "&#x25BA; Question", label = NULL) {
    question(text = "", begin = begin, label = label)
}

#' @export
question <- function(text = "", begin = "&#x25BA; Question", label = NULL) {
    
    if (knitr::is_html_output()) {
        
        label <- ifelse(is.null(label), "", label)
    
        part1 <- paste0(
            sprintf("<div class = 'question' id='%s'>", label),
            "<p class='question-begin'>", begin, "</p>",
            "<div class='question-body'>")
        
        if(nchar(text) == 0) {
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
