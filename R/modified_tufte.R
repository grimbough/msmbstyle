# taken from Tufte version 0.12.0
parse_footnotes <- function(x, fn_label = "fn") {
    i <- grep('^<div class="footnotes[^"]*"[^>]*>', x)
    if (length(i) == 0) {
        return(list(items = character(), range = integer()))
    }
    j <- which(x == "</div>")
    j <- min(j[j > i])
    #n <- length(x)
    r <- sprintf(
        '<li id="%s([0-9]+)"><p>(.+)<a href="#%sref\\1"([^>]*)>.{1,2}</a></p></li>',
        fn_label, fn_label
    )
    s <- paste(x[i:j], collapse = "\n")
    list(
        items = gsub(r, "\\2", unlist(regmatches(s, gregexpr(r, s)))),
        range = i:j
    )
}
