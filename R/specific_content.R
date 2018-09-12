
toggle_func2 <- "<script>
function toggle_visibility(id1, id2, id3) {
var e = document.getElementById(id1);
var f = document.getElementById(id2);
var g = document.getElementById(id3);

e.style.display = ((e.style.display!='none') ? 'none' : 'block');
g.style.display = ((g.style.display!='none') ? 'none' : 'block');

if(f.classList.contains('fa-plus-square')) {
    f.classList.add('fa-minus-square')
    f.classList.remove('fa-plus-square')
} else {
    f.classList.add('fa-plus-square')
    f.classList.remove('fa-minus-square')
}

}
</script>"

#' @export
solution <- function(text = "", begin = "&#x25BA; Solution", toggle = TRUE) {
    
    if (knitr::is_html_output()) {
        id1 <- generate_id()
        id2 <- generate_id()
        id3 <- generate_id()
        
        toggle_code <- ifelse(toggle, 
                              toggle_func2,
                              "")
        
        paste0(toggle_code,
               "<div class = \"solution\">",
               "<p class=\"solution-begin\">", begin, 
               ifelse(toggle, 
                      sprintf("<span id='%s' class=\"fa fa-plus-square solution-icon clickable\" onclick=\"toggle_visibility('%s', '%s', '%s')\"></span>", id1, id2, id1, id3), 
                      ""),
               "</p>",
               "<p class=\"solution-body\" id = \"", id2,"\" style=\"display: none;\">", text, "</p>",
               "<p class=\"solution-end\" id = \"", id3, "\"style=\"display: none;\">", 
               "<span class=\"fa fa-square-o solution-icon\">", "</p>",
               "</div>")
    }    # TODO
    #else if (knitr::is_latex_output()) {
    #    
    #}
    else {
        warning("solution() only works for HTML output", 
                call. = FALSE)
        text
    }
    
}

#' @export
question <- function(text = "", label = NULL, begin = "&#x25BA; Question") {
    
    if (knitr::is_html_output()) {
        
        label <- ifelse(is.null(label), "", label)
    
        paste0(
            sprintf("<div class = \"question\" id='%s'>", label),
            "<p class=\"question-begin\">", begin, "</p>",
            "<p class=\"question-body\">", text, "</p>",
            "<p class=\"question-end\">", "<span class=\"fa fa-square-o solution-icon\">", "</p>",
            "</div>"
        )
            
    
    }# TODO
    #else if (knitr::is_latex_output()) {
    #    
    #}
    else {
        warning("question() only works for HTML output", 
                call. = FALSE)
        text
    }
}
