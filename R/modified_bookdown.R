
resolve_refs_html = function(content, global = FALSE) {
    content = bookdown:::resolve_ref_links_html(content)
    
    res = parse_ques_labels(content, global)
    content = res$content
    ref_table = c(res$ref_table)
    
    # look for @ref(ques:) and resolve to actual question numbers
    m = gregexpr('(?<!\\\\)@ref\\(((?:ques):[-[:alnum:]]+)\\)', content, perl = TRUE)
    refs = regmatches(content, m)
    for (i in seq_along(refs)) {
        if (length(refs[[i]]) == 0) next
        ref = bookdown:::ref_to_number(refs[[i]], ref_table, FALSE)
        refs[[i]] = ref
    }
    regmatches(content, m) = refs
    content
}

## we are assigning numbers to each question, based on the chapter they
## reside in.
parse_ques_labels <- function(content, global = FALSE) {
    reg_chap = '^(<h1><span class="header-section-number">)([A-Z0-9]+)(</span>.+</h1>)$'
    lines = grep(reg_chap, content)
    chaps = gsub(reg_chap, '\\2', content[lines])  # chapter numbers
    if (length(chaps) == 0) global = TRUE  # no chapter titles or no numbered chapters
    arry = character()  # an array of the form c(label = number, ...)
    if (global) chaps = '0'  # Chapter 0 (could be an arbitrary number)

    # look for (#fig:label) or (#tab:label) and replace them with Figure/Table x.x
    m = gregexpr('id="ques:[-[:alnum:]]+"', content)
    labs = regmatches(content, m)
    cntr = bookdown:::new_counters('ques', chaps)  # chapter counters

    for (i in seq_along(labs)) {
        lab = labs[[i]]
        if (length(lab) == 0) next
        if (length(lab) > 1)
            stop('There are multiple labels on one line: ', paste(lab, collapse = ', '))
        
        j = if (global) chaps else tail(chaps[lines <= i], 1)
        lab = gsub('^id="|"$', '', lab)
        type = gsub('^([^:]+):.*', '\\1', lab)
        num = arry[lab]
        if (is.na(num)) {
            num = cntr$inc(type, j)  # increment number only if the label has not been used
            if (!global) num = paste0(j, '.', num)  # Figure X.x
        }
        arry = c(arry, setNames(num, lab))
        
        ## the actual header is 2 lines later than the id tag
        content[i + 2] <- paste(content[i + 2], num, sep = " ")
    }
    
    list(content = content, ref_table = arry)
}