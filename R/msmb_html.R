#' @rdname msmb_html
#' @importFrom bookdown html_chapters
#' @export
msmb_html_book = function(...) {
    html_chapters(..., 
                  base_format = msmbstyle::msmb_html, 
                  page_builder = msmb_build_chapter)
}

#' Modern Statstics for Modern Biology HTML format
#' 
#' Template for creating webpages in the style of Modern Statistics for
#' Modern Biology \url{http://www-huber.embl.de/msmb/}.
#' 
#' @details \code{msmb_html()} provides the HTML format used in 
#' Modern Statistics for Modern Biology:
#'   \url{http://www-huber.embl.de/msmb/}.
#' @details \code{msmb_html_book()} is used to create output split
#' into individual chapters for a book.  If you wish to create a complete
#' book this should be used in the output section of your YAML header
#' in the R Markdown document. 
#' 
#' @param margin_references \code{logical}.  Determines whether to place 
#'   citations in the margin, or collate them at the end of the document.
#' @rdname msmb_html
#' @export
msmb_html = function(
  ..., 
  margin_references = TRUE
) {

  ##tufte_variant = "envisioned"
  tufte_variant = "default"    
  tufte_features = character()
  
  html_document2 = function(..., extra_dependencies = list()) {
    rmarkdown::html_document(
      ..., extra_dependencies = c(
        extra_dependencies, 
        list(bookdown:::jquery_dependency()),
        tufte_html_dependency(tufte_features, tufte_variant), 
        msmb_html_dependency()
      )
    )
  }
  format = html_document2(theme = NULL, ...)
  pandoc2 = pandoc2.0()

  # when fig.margin = TRUE, set fig.beforecode = TRUE so plots are moved before
  # code blocks, and they can be top-aligned
  ohooks = knitr::opts_hooks$set(fig.margin = function(options) {
    if (isTRUE(options$fig.margin)) options$fig.beforecode = TRUE
    options
  })

  # make sure the existing post processor is called first in our new processor
  post_processor = format$post_processor
  format$post_processor = function(metadata, input, output, clean, verbose) {

    if (is.function(post_processor))
      output = post_processor(metadata, input, output, clean, verbose)

    knitr::opts_hooks$restore(ohooks)

    x = xfun::read_utf8(output)
    fn_label = paste0(knitr::opts_knit$get('rmarkdown.pandoc.id_prefix'), 'fn')
    footnotes = tufte:::parse_footnotes(x, fn_label)
    notes = footnotes$items
    # replace footnotes with sidenotes
    for (i in seq_along(notes)) {
      num = sprintf(
        '<a href="#%s%d" class="%s" id="%sref%d"><sup>%d</sup></a>',
        fn_label, i, if (pandoc2) 'footnote-ref' else 'footnoteRef', fn_label, i, i
      )
      con = sprintf(paste0(
        '<label for="tufte-sn-%d" class="margin-toggle sidenote-number">%d</label>',
        '<input type="checkbox" id="tufte-sn-%d" class="margin-toggle">',
        '<span class="sidenote"><span class="sidenote-number">%d</span> %s</span>'
      ), i, i, i, i, notes[i])
      x = gsub_fixed(num, con, x)
    }
    # remove footnotes at the bottom
    if (length(footnotes$range)) x = x[-footnotes$range]

    # replace citations with margin notes
    if (margin_references) x = tufte:::margin_references(x)

    # place figure captions in margin notes
    #x[x == '<p class="caption">'] = '<p class="caption marginnote shownote">'

    # move </caption> to the same line as <caption>; the previous line should
    # start with <table
    for (i in intersect(grep('^<caption>', x), grep('^<table', x) + 1)) {
      j = 0
      while (!grepl('</caption>$', x[i])) {
        j = j + 1
        x[i] = paste0(x[i], x[i + j])
        x[i + j] = ''
      }
    }
    
    x <- .arrange_tables(x)

    # add an incremental number to the id of <label> and <input> for margin notes
    r = '(<label|<input type="checkbox") (id|for)(="tufte-mn)-(" )'
    m = gregexpr(r, x)
    j = 1
    regmatches(x, m) = lapply(regmatches(x, m), function(z) {
      n = length(z)
      if (n == 0) return(z)
      if (n %% 2 != 0) warning('The number of labels is different with checkboxes')
      for (i in seq(1, n, 2)) {
        if (i + 1 > n) break
        z[i + (0:1)] =  gsub(r, paste0('\\1 \\2\\3-', j, '\\4'), z[i + (0:1)])
        j <<- j + 1
      }
      z
    })
    
    x = .add_meta_tags(x, md_file = input)
    x = .toc_2_navbar(x, md_file = input)
    x = .catch_sourceCode(x) %>%
        .catch_questions() %>%
        .apply_rows2() %>%
        .clean_columns()


    xfun::write_utf8(x, output)
    output
  }

  if (is.null(format$knitr$knit_hooks)) format$knitr$knit_hooks = list()
  
  format$knitr$knit_hooks$plot = function(x, options) {
    # make sure the plot hook always generates HTML code instead of ![]()
    if (is.null(options$out.extra)) options$out.extra = ''
    fig_margin = isTRUE(options$fig.margin)
    fig_fullwd = isTRUE(options$fig.fullwidth)
    if (fig_margin || fig_fullwd) {
      if (is.null(options$fig.cap)) options$fig.cap = ' ' # empty caption
    } else if (is.null(options$fig.topcaption)) {
      # for normal figures, place captions at the top of images
      #options$fig.topcaption = TRUE
    }
    res = knitr::hook_plot_md(x, options)
    if (fig_margin) {
            res = gsub_fixed('<div class="figure">', '<div class="col-sm-3 col-sm-push-9">', res)
            res = gsub_fixed('<p class="caption">', '<div class="caption">', res)
            res = gsub_fixed('</p>', '</div>', res)
        # }
    } else if (fig_fullwd) {
      res = gsub_fixed('<div class="figure">', '<div class="figure col-sm-12">', res)
      res = gsub_fixed('<p class="caption">', '<p class="caption col-sm-3 col-sm-push-9">', res)
    } else { ## normal figures
        res = gsub_fixed('<div class="figure">', '<div class="col-sm-9">', res) 
        res = gsub_fixed('<p class="caption">', '</div>\n<div class="caption col-sm-3">', res)
        res = gsub_fixed('</p>', '</div>', res)
    }
    res
  }
  
  format$knitr$knit_hooks$chunk = function(x, options) {
      
      res <- x
      ## we only want to add a row if this isn't margin content
      if(!stringr::str_detect(options$engine, "marginfigure")) {
        res <- paste0("<div class='row test'>", res)
        if(str_detect(res, "<div class='ques-sol'")) {
            ## close chunk row before starting question
            res <- gsub_fixed("<div class='ques-sol'", "</div><div class='ques-sol'", x = res)
      #  } else if (str_detect(res, "<table>")) {
      #      res <- gsub_fixed("<table>", "</div><table>", x = res)
        } else {
            res <- paste0(res, '</div>')
        }
      }
      
      chunk_marks <- stringr::str_locate_all(res, "```")[[1]]
      
      ## margin figures come before code, so this column is shifted left
      col_div <- ifelse(isTRUE(options$fig.margin),
                        '<div class="col-sm-9 col-sm-pull-3">\n```',
                        '<div class="col-sm-9">\n```')
      
      for(i in seq_len(nrow(chunk_marks))) {
          chunk_mark <- stringr::str_locate_all(res, "```")[[1]][i,]
          stringr::str_sub(res, chunk_mark[1], chunk_mark[2]) <- ifelse(i %% 2, 
                                                              col_div,
                                                              '```\n</div>')
      }
      res
  }

  ## engine for placing arbitrary content in the margin
  knitr::knit_engines$set(marginfigure = function(options) {
      options$type = 'col-sm-3 margin2'
      if (is.null(options$html.tag)) options$html.tag = 'div'
      eng_block = knitr::knit_engines$get('block')
      eng_block(options) %>%
          stringr::str_replace_all('<(/?)p>', '<\\1div>') %>%
          paste('<div class="row">', . , '</div>', sep = "\n") ## we add a tag that closes a row defined elsewhere
      
  })

  format$inherits = 'html_document'

  format
}

## Slightly modified version of tufte:::tufte_html_dependency
## Modified to specify the package name the CSS files are found in
## TODO Can we just use the version in tufte itself?
#' @importFrom htmltools htmlDependency
tufte_html_dependency = function(features, variant) {
  list(htmlDependency(
    'tufte-css', '2015.12.29',
    src = template_resources('tufte_html', package = 'tufte'), stylesheet = c(
      sprintf('tufte-%s.css', features), 'tufte.css',
      if (variant != 'default') sprintf('%s.css', variant)
    )
  ))
}

#' @importFrom htmltools htmlDependency
msmb_html_dependency = function() {
    list(#bookdown:::jquery_dependency(),
         htmlDependency('msmb-css', version = '0',
                        src = template_resources('msmb_html', package = 'msmbstyle'), 
                        stylesheet = 'msmb.css')
         )
}

## Identifies any <h2> headings in the output HTML (equivalent to a section)
## Builds a table of contents for the current page based on these
## that will be included in the drop-down navigation
#' @importFrom stringr str_detect str_match str_trim
#' @importFrom xml2 xml_text xml_find_all
.create_section_links <- function(html_lines, include_nums = TRUE, target = "panel") {
    
    html <- xml2::read_html(paste(html_lines, collapse="\n"))
    
    section_nodes <- xml2::xml_find_all(html, xpath = "//h2")
    if(!length(section_nodes)) {
        return(NULL)
    }
    
    section_names <- xml_text(section_nodes) %>% 
        str_trim()
    if(!include_nums)
        section_names <- stringr::str_remove(string = section_names, "^[0-9\\.]+ ")
    
    section_links <- html_lines[str_detect(html_lines, 'class="section level2')] %>%
        str_match(pattern = 'id="([[:alnum:]-:]+)"')
    section_links <- section_links[,2]
    
    # error condition if we've missed a link or section
    if(length(section_names) != length(section_links)) 
        stop('Not same length')
    
    if(target == "panel") {
        tmp <- c('<div class="panel panel-default toc-section">',
                 '<div class="panel-heading">Chapter Navigation</div>',
                 '<div class="section-link list-group">',
                 paste0('<a class="list-group-item" href="#', section_links, '">', section_names, '</a>'),
                 '</div>',
                 '</div>')
    } else if (target == "navbar") {
        tmp <- paste0('<li><a href="#', section_links, '">', section_names, '</a></li>')
    } else {
        stop("Unknown target")
    }
    
    return(paste(tmp, collapse = '\n'))
    
}

.apply_rows <- function(x) {
    
    x <- x %>%
       # stringr::str_replace_all('<h1>', '<div class="row">\n<div class="col-sm-9"><h1>') %>%
       # stringr::str_replace_all('</h1>', '</h1></div>\n</div>') %>%
       # stringr::str_replace_all('<h2>', '<div class="row">\n<div class="col-sm-9"><h2>') %>%
       # stringr::str_replace_all('</h2>', '</h2></div>\n</div>') %>%
       # stringr::str_replace('<p([> ])', '<div class="row">\n<div class="col-sm-9"><p\\1') %>%
       # stringr::str_replace('</p>', '</p></div>\n</div>') %>%
        stringr::str_replace_all('<div class="row">\n<div class="col-sm-9"><p>(<img [[:print:]]+)</p></div>\n</div>',
                                 '<p>\\1</p>')
    
    return(x)
    
}

#' @importFrom xml2 xml_find_all xml_has_attr xml_attrs xml_add_parent xml_parent
.apply_rows2 <- function(x) {
    
    html <- paste0(x, collapse = "\n") %>%
        read_html()
    nodes <- xml2::xml_find_all(html, xpath = "//table|//p|//h1|//h2|//h3|//h4")
    for(i in seq_along(nodes)) {
        node <- nodes[i]
        if( !xml_has_attr(node, "class") && 
            !grepl(pattern = 'col-sm-', x = xml_attrs(xml_parent(node)) ) ) {
            xml_add_parent(node, "div", class = "row") 
            xml_add_parent(node, "div", class = "col-sm-9") 
        }
    }
    stringr::str_split(as.character(html), pattern = "\n")[[1]]
}

.catch_sourceCode <- function(x) {
    
    html <- paste0(x, collapse = "\n") %>%
        read_html()
    code_nodes <- xml2::xml_find_all(html, xpath = "//div[starts-with(@class, 'sourceCode')]")
    for(i in seq_along(code_nodes)) {
        node <- code_nodes[i]
        if( !grepl(pattern = 'col-sm-', x = xml_attrs(xml_parent(node)) ) ) {
            xml_add_parent(node, "div", class = "row") 
            xml_add_parent(node, "div", class = "col-sm-9") 
        }
    }
    stringr::str_split(as.character(html), pattern = "\n")[[1]]
}

.catch_questions <- function(x) {
    
    html <- paste0(x, collapse = "\n") %>%
        read_html()
    code_nodes <- xml2::xml_find_all(html, xpath = "//span[starts-with(@class, 'question-')]|//span[starts-with(@class, 'solution-')]")
    for(i in seq_along(code_nodes)) {
        node <- code_nodes[i]
        if( !grepl(pattern = 'col-sm-', x = xml_attrs(xml_parent(node)) ) ) {
            xml_add_parent(node, "div", class = "row") 
            xml_add_parent(node, "div", class = "col-sm-9") 
        }
    }
    stringr::str_split(as.character(html), pattern = "\n")[[1]]
}

#' @import xml2
#' @importFrom stringr str_remove
.clean_columns <- function(x) {
    
    html <- paste0(x, collapse = "\n") %>%
        read_html()
    col_nodes <- xml2::xml_find_all(html, xpath = "//div[starts-with(@class, 'col')]")
    for(i in seq_along(col_nodes)) {
        node <- col_nodes[i]
        if( any(grepl(pattern = 'col-sm-9', x = xml_attrs(xml_parents(node)) )) ) {
            xml2::xml_set_attr(node, 'class', 'col-sm-12')
        }
    }
    
    ## items can be in the margin with no content next to it e.g. 
    ## code with echo=FALSE. Here we move this content to the row above
    margin_nodes <- xml_find_all(html, xpath = "//div[contains(@class, 'col-sm-3')]")
    for(i in seq_along(margin_nodes)) {
        node <- margin_nodes[i]
        has_siblings <- any(unlist(lapply(xml_attrs(xml_siblings(node)), 
                                  grepl, pattern = "col-sm-9")))
        ## if there is a col-sm-9 sibling thats ok
        ## move our node if it is an only child
        if(!has_siblings) {
            parent <- xml_parent(node)
            uncle <- rev(xml_find_all(parent, "preceding-sibling::*"))[1]
            #where <- ifelse(str_detect(xml_attrs(node), "col-sm-push-[0-9]+"),
            #                0, length(xml_children(uncle)))
            xml_set_attr(node, 'class', 
                         str_remove(xml_attr(node, 'class'), " ?col-sm-push-[0-9]+"))
            xml_add_child(uncle, node)
            xml_remove(parent)
        }
    }
    
    stringr::str_split(as.character(html), pattern = "\n")[[1]]
}

## Converts the table of contents HTML produced by bookdown into
## the format required for the drop-down menu navigation.
#' @importFrom stringr str_replace_all str_replace str_c
#' @importFrom magrittr %>%
.toc_2_navbar <- function(x, md_file) {
    
    #remove the bookdown inclusion of header etc
    head_idx <- which(str_detect(x, pattern = "bookdown:title:(start|end)"))
    if(length(head_idx) == 2) 
        x <- x[-((head_idx[1]+1):(head_idx[2]-1))]
    
    yaml <- xfun::read_utf8(md_file) %>%
            bookdown:::fetch_yaml() %>%
            rmarkdown:::parse_yaml_front_matter()
    
    title_and_author <- paste0('<span class="title hidden-xs">', yaml$title, '</span><br />',
          '<span class="author hidden-xs">', paste(yaml$author, collapse = ', '), '</span>')
    
    toc_start <- min(which(str_detect(x, '<ul>')))
    toc_end <- min(which(str_detect(x, '</ul>')))

    chapter_links <- x[(toc_start+1):(toc_end-1)]
    #section_links <- .create_section_links(x, target = "navbar")
    
    x[toc_start:toc_end] <- ''
    x[toc_start] <- paste(c('<nav class="navbar navbar-default">
        <div class="container-fluid">
        <!-- Brand and toggle get grouped for better mobile display -->
        <div class="navbar-header">
        <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        </button>
        <span class="title-collapsed visible-xs">', yaml$title, '</span>
        </div>
        
        <!-- Collect the nav links, forms, and other content for toggling -->
        <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
        <ul class="nav navbar-nav hidden-xs">
        <!-- title and authors -->',
                            title_and_author,
        '</ul>
        <ul class="nav navbar-nav navbar-right">
        <li class="dropdown">
        <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Chapters <span class="caret"></span></a>
        <ul class="dropdown-menu">
            <!-- links to chapters here -->',
                           chapter_links,
        '</ul>
        </li>

        <li class="dropdown hidden-md hidden-lg">
        <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Sections <span class="caret"></span></a>
        <ul class="dropdown-menu">
            <!-- links to sections here -->',
                            #section_links,
        '</ul>
        </li>
        </ul>
        </div><!-- /.navbar-collapse -->
        </div><!-- /.container-fluid -->
        </nav>'),
        collapse = "\n")
    return(x)
}

msmb_build_chapter = function(
    head, toc, chapter, link_prev, link_next, rmd_cur, html_cur, foot
) {
    
    ## insert script for solution toggle
    ## we put it before the msmb.css & after jQuery
    msmb_css_line <- max(str_which(head, "msmb.css\""))
    head[msmb_css_line] <- paste(toggle_script(), head[msmb_css_line], sep = "\n")
    
    chapter_nav <- .create_section_links(chapter, include_nums = TRUE, target = "navbar")
    chapter_sidebar <- .create_section_links(chapter, include_nums = TRUE, target = "panel")
    
    toc = str_replace_all(toc, 
                          pattern = 'href="([[:alnum:]:-]+.html)?#[[:alpha:]:-]+', 
                          replacement = 'href="\\1')
    
    if(!is.null(chapter_nav)) 
        toc <- str_replace(toc, "<!-- links to sections here -->", chapter_nav)
    
    chapter <- .apply_rows(chapter) %>%
        .put_marginfig_in_row() #%>%
        #.move_margin_notes()
    chapter <- .number_questions(chapter)
    chapter_body <- .nonumber_chap_figs(chapter)
    
    
    paste(c(
        head,
        '<div class="navbar-fixed-top msmb-header">',
          '<div class="row">',
              toc,
          '</div>',
        '</div>',
        '<div class="container chapter-content">',
          '<div class="row">',
            '<div class="hidden-xs hidden-sm col-md-2">',
              chapter_sidebar,
            '</div>',
            '<div class="col-xs-12 col-md-10">',
              chapter_body,
            '</div>',
          '</div>',
          '<div class="row">',
            '<div class="col-xs-12">',
              '<p style="text-align: center;">',
                bookdown:::button_link(link_prev, 'Previous'),
                bookdown:::button_link(link_next, 'Next'),
              '</p>',
              '<p class="build-date">Page built: ', as.character(Sys.Date()), '</p>',
            '</div>',
          '</div>',
        '</div>',
        foot
    ), collapse = '\n')
}

#' @importFrom stringr str_match_all str_which str_replace
#' @importFrom magrittr extract
.number_questions <- function(chapter) {
    
    chap_num <- stringr::str_match_all(chapter, pattern = "class=\"header-section-number\">([0-9]+)") %>% 
        unlist() %>%
        magrittr::extract(2)
    
    question_divs <- stringr::str_which(chapter, "id=\"ques:")
    if(!length(question_divs)) { return(chapter) }
    ## for now assume there are always two lines between these
    question_heads <- question_divs + 1
    
    chapter[question_heads] <- mapply(FUN = function(x, y, chapter, chap_num) { 
        str_replace(chapter[x], "</span>", paste0(" ", chap_num, ".", y, "</span>")) },
        question_heads, seq_along(question_heads),
        MoreArgs = list(chapter = chapter, chap_num = chap_num))
    
    question_labs <- str_match(chapter[question_divs], "id=\"(ques:[[:alnum:]-]+)\"")[,2]
    for(i in seq_along(question_labs)) {
        ref_lines <- stringr::str_which(chapter, paste0("<a href=\".*#", question_labs[i], "\">"))
        chapter[ref_lines] <- str_replace(chapter[ref_lines], 
                                          "\\?\\?",
                                          paste0(chap_num, "\\.", i))
    }
    
    return(chapter)
}

## if chapter is the first in a book, but is unnumbered e.g. an introduction
## the figures will also have no numbers i.e. "Figure ."
## This function finds them and labels Figure 1, Figure 2, etc
#' @importFrom xml2 read_xml read_html xml_find_all xml_replace
#' @importFrom stringr fixed
.nonumber_chap_figs <- function(chapter) {
    
    ## don't do anything if the missing figure numbers aren't found
    if(!any(stringr::str_detect(chapter, pattern = "Figure \\."))) { return(chapter) }
    
    chapter2 <- paste0(chapter, collapse = "\n")
    
    html <- xml2::read_html(chapter2)
    tmp <- xml2::xml_find_all(html, 
                              xpath = "//div[starts-with(@class, 'figure')]|//span[starts-with(@class, 'marginnote')]")
    global_pattern <- global_replacement <- NULL
    
    curr_fig <- 1
    for(i in seq_along(tmp)) {
        x <- as.character(tmp[i])
        
        ## might be a marginnote without a figure - skip these
        if(!stringr::str_detect(x, 'Figure \\.:')) { next; }

        caption <- stringr::str_match(x, ".*(Figure \\.: [[:print:]\n]+)</p>")[1,2]
        caption_new <- stringr::str_replace(caption, "Figure \\.", sprintf("Figure %s", curr_fig))
        chapter2 <- stringr::str_replace(chapter2, 
                                         pattern = stringr::fixed(caption), 
                                         replacement = caption_new)
        
        id <- stringr::str_match(x, "id=\"(fig:[[:graph:]- ]+)\"")[1,2]
        global_pattern <- c(global_pattern, paste0("#", id, "\">."))
        global_replacement <- c(global_replacement, paste0("#", id, "\"> ", i))
        curr_fig <- curr_fig + 1
    }
    
    ## reaplce references throughout the chapter
    names(global_replacement) <- global_pattern
    chapter3 <- stringr::str_replace_all(chapter2, global_replacement)
    stringr::str_split(chapter3, "\n")[[1]]

}

## Add additional <meta> tags e.g. cover image to display in links
#' @importFrom stringr str_which
#' @importFrom xfun read_utf8
.add_meta_tags <- function(x, md_file) {
    
    og_type_line <- str_which(x, "<meta property=\"og:type\"")
    
    yaml <- xfun::read_utf8(md_file) %>%
        bookdown:::fetch_yaml() %>%
        rmarkdown:::parse_yaml_front_matter()
    
    ## include the cover image if there is one
    if(!is.null(yaml$cover) && length(og_type_line)) { 
        og_image_tag <- paste0('<meta property="og:image" content="',
               yaml$cover, '" />') 
        x[og_type_line] <- paste(x[og_type_line], og_image_tag, "\n")
    }
    
    return(x)
}

## find any instance of a marginfigure inserted by a code chunk
## and remove the </div> above it, we have already provided a replacement
.put_marginfig_in_row <- function(x) {
    
#    idx <- str_which(x, '<div class="col-sm-3 margin2">')
#    if(length(idx)) {
#        x[idx-1] <- stringr::str_remove(x[idx-1], '</div>$')
#    }
    x
}

## moves margin_note entries outside the main column <div>
#' @importFrom stringr str_which fixed str_replace str_trim str_match
#' @importFrom markdown renderMarkdown
.move_margin_notes <- function(x) {
    
    margin_note_idx <- stringr::str_which(x, '<!--MOVE')
    
    for(i in seq_along(margin_note_idx)) {
        idx <- margin_note_idx[i]
        margin_note_md <- stringr::str_match(x[idx], "<!--MOVE<div class=col-sm-3>(.*)</div>EVOM-->")[1,2]
        margin_note_html <- markdown::renderMarkdown(text = margin_note_md) %>%
            stringr::str_trim()
        
        x[idx] <- stringr::str_replace(x[idx], fixed(margin_note_md), margin_note_html)
        x[idx] <- stringr::str_replace(x[idx], 
                                       "(.*)<!--MOVE(.*)EVOM-->(.*)", 
                                       "\\1\\3\\2")
    }
    return(x)
}

#' @export
margin_note <- function(text) {
    
    paste0('<div class="row"><div class=col-sm-3 margin-note>',
          '<span class="glyphicon glyphicon-info-sign" aria-hidden="true"></span> ',
          text,
          '</div></div>')
    
}

## for main body tables, we place the caption in the margin.
## for tables with class='margintab' we put both the table
## and caption in the margin.
.arrange_tables <- function(x) {
    
    # move </caption> to the same line as <caption>; the previous line should
    # start with <table
    for (i in intersect(grep('^<caption>', x), grep('^<table', x) + 1)) {
        j = 0
        while (!grepl('</caption>$', x[i])) {
            j = j + 1
            x[i] = paste0(x[i], x[i + j])
            x[i + j] = ''
        }
    }
    
    # place table captions in the margin
    # keep the commented <!--<caption>--> tags, these are used to number tables
    r = '^<caption>(.+)</caption>$'
    tab_close <- str_which(x, "</table>")
    for (i in grep(r, x)) {
        # the previous line should be <table> or <table class=...>
        if (!grepl('^<table( class=.+)?>$', x[i - 1])) next
        
        cap = gsub(r, '\\1', x[i])            
        x[i] = x[i - 1]
        if(!grepl('class ?=.*margintab ?', x[i - 1])) { ## not a margin table
            x[i - 1] = paste0(
                '</div>',
                '<div class="row">',
                '<!--\n<caption>-->',
                '<div class="col-sm-3 col-sm-push-9">',
                cap, '</div><!--</caption>-->',
                '<div class="col-sm-9 col-sm-pull-3">'
            )
            ## close the row div after the table
            k <- min(tab_close[tab_close > i])
            x[k] <- gsub("</table>", "</table></div>", x[k])
        } else {
            x[i-1] = paste0(
                '<div class="col-sm-3">'
            )
            k <- min(tab_close[tab_close > i])
            x[k] <- gsub("</table>", paste0("</table><!--\n<caption>-->", cap, "</div><!--</caption>-->"), x[k])
        }
    }
    return(x)
}    