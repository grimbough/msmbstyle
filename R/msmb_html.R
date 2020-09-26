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
#' @importFrom bookdown resolve_refs_html
#' @export
msmb_html = function(
  ..., 
  margin_references = TRUE
) {

  tufte_variant = "envisioned"
  tufte_features = character()
  
  html_document2 = function(..., extra_dependencies = list()) {
    rmarkdown::html_document(
      ..., extra_dependencies = c(
        extra_dependencies, 
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
    
    x = resolve_refs_html(x)
    
    fn_label = paste0(knitr::opts_knit$get('rmarkdown.pandoc.id_prefix'), 'fn')
    footnotes = parse_footnotes(x, fn_label)
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
    x[x == '<p class="caption">'] = '<p class="caption marginnote shownote">'

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
    r = '^<caption>(.+)</caption>$'
    for (i in grep(r, x)) {
      # the previous line should be <table> or <table class=...>
      if (!grepl('^<table( class=.+)?>$', x[i - 1])) next
      cap = gsub(r, '\\1', x[i])
      x[i] = x[i - 1]
      x[i - 1] = paste0(
        '<p><!--\n<caption>-->', '<span class="marginnote shownote">',
        cap, '</span><!--</caption>--></p>'
      )
    }

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
    
    # remove hardcoded sourceCode styling
    x = stringr::str_remove(x, fixed(".sourceCode { overflow: visible; }"))
    
    x = .toc_2_navbar(x, md_file = input)

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
      options$fig.topcaption = TRUE
    }
    res = knitr::hook_plot_md(x, options)
    if (fig_margin) {
      res = gsub_fixed('<p class="caption">', '<!--\n<p class="caption marginnote">-->', res)
      res = gsub_fixed('</p>', '<!--</p>-->', res)
      res = gsub_fixed('</div>', '<!--</div>--></span></p>', res)
      res = gsub_fixed(
        '<div class="figure">', paste0(
          '<p>', '<span class="marginnote shownote">', '\n<!--\n<div class="figure">-->'
        ), res
      )
    } else if (fig_fullwd) {
      res = gsub_fixed('<div class="figure">', '<div class="figure fullwidth">', res)
      res = gsub_fixed(
        '<p class="caption">', '<p class="caption marginnote shownote">', res
      )
    }
    res
  }

  knitr::knit_engines$set(marginfigure = function(options) {
    options$type = 'marginnote'
    if (is.null(options$html.tag)) options$html.tag = 'span'
    options$html.before = tufte:::marginnote_html()
    eng_block = knitr::knit_engines$get('block')
    eng_block(options)
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
    list(htmlDependency(
        'msmb-css', version = '0',
        src = template_resources('msmb_html', package = 'msmbstyle'), stylesheet = 'msmb.css'
    ))
}

## Identifies any <h2> headings in the output HTML (equivalent to a section)
## Builds a table of contents for the current page based on these
## that will be included in the drop-down navigation
#' @importFrom stringr str_detect str_match
.create_section_links <- function(html_lines, include_nums = TRUE) {
    
    section_names <- html_lines[str_detect(html_lines, '<h2>')] %>%
        str_match('>([0-9.]+)</span>(.*)</h2>') 
    ## we have to treat the intro page differently as it is unnumbered
    if(all(is.na(section_names))) {
        section_names <- html_lines[str_detect(html_lines, '<h2>')] %>% 
            str_match('>([0-9.]*)(.*)</h2>')
    }
    
    if(!nrow(section_names)) {
        return(NULL)
    }
    
    if(include_nums)
        section_names <- paste0(section_names[,2], section_names[,3])
    else 
        section_names <- section_names[,3]
    
    section_links <- html_lines[str_detect(html_lines, 'class="section level2')] %>%
        str_match(pattern = 'id="([[:alnum:]-:]+)"')
    section_links <- section_links[,2]
    
    # error condiction if we've missed a link or section
    if(length(section_names) != length(section_links)) 
        stop('Not same length')
    
    tmp <- c('<ul class="toc-sections">', 
             paste0('<li class="toc"><a href="#', section_links, '">', section_names, '</a></li>'),
             '</ul>')
    
    return(paste(tmp, collapse = '\n'))
    
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
    
    header <- paste0('<p class="title">', yaml$title,
                     '<p><p class="author">', 
                     paste(yaml$author, collapse = ', '), '</p>')

    toc_start <- str_which(x, pattern = "<!--bookdown:toc:start-->")
    toc_end <- str_which(x, pattern = "<!--bookdown:toc:end-->")
    x[toc_start:toc_end] <- x[toc_start:toc_end] %>%
        str_replace('<ul>',
                    paste0('<ul class="navbar">\n',
                           '<li class="msmb">', header, '</li>\n',
                           '<li class="dropdown" style="float:right">\n',
                           '<a href="javascript:void(0)" class="dropbtn">&#x25BE; Chapters</a>\n',
                           '<div class="dropdown-content">')) %>%
        str_replace_all('<li>', '') %>% 
        str_replace_all('</li>', '') %>%
        str_replace('</ul>', '</div>\n</li>\n</ul>')
    
    return(x)
}


msmb_build_chapter = function(
    head, toc, chapter, link_prev, link_next, rmd_cur, html_cur, foot
) {
    
    ## insert script for solution toggle
    ## we put it after the msmb.css as this should always be present
    last_script <- max(str_which(head, "msmb.css\""))
    head[last_script] <- paste(head[last_script], toggle_script(), sep = "\n")
    
    # add a has-sub class to the <li> items that has sub lists
    toc = gsub('^(<li>)(.+<ul>)$', '<li class="has-sub">\\2', toc)
    
    toc = str_replace_all(toc, 
                          pattern = 'href="([[:alnum:]:-]+.html)?#[[:alnum:]:-]+', 
                          replacement = 'href="\\1')
    
    # manipulate the TOC for this page to include sections
    this_page_idx <- str_which(toc, html_cur)
    if(length(this_page_idx)) {
        this_page = min(this_page_idx)
        toc[ this_page ] <- toc[ this_page ] %>%
                 str_replace('href', 'id="active-page" href') %>%
                 str_c(.create_section_links(chapter, include_nums = FALSE))
    }
    
    #chapter <- .number_questions(chapter)
    chapter <- .nonumber_chap_figs(chapter)
    chapter <- .retag_margin_figures(chapter)
    chapter <- .move_margin_table(chapter)
    
    paste(c(
        head,
        '<div class="row">',
        '<div class="col-sm-12">',
        toc,
        '</div>',
        '</div>',
        '<div class="row">',
        '<div class="col-sm-12">',
        chapter,
        '<p style="text-align: center;">',
        bookdown:::button_link(link_prev, 'Previous'),
        bookdown:::source_link(rmd_cur),
        bookdown:::button_link(link_next, 'Next'),
        '</p>',
        '<p class="build-date">Page built: ', as.character(Sys.Date()), ' using ', R.version.string, '</p>',
        '</div>',
        '</div>',
        foot
    ), collapse = '\n')
}

#' @importFrom stringr str_match_all str_which
#' @importFrom magrittr extract
.number_questions <- function(chapter) {
    
    chap_num <- stringr::str_match_all(chapter, pattern = "class=\"header-section-number\">([0-9]+)") %>% 
        unlist() %>%
        magrittr::extract(2)
    
    question_divs <- stringr::str_which(chapter, "id=\"ques:")
    if(!length(question_divs)) { return(chapter) }
    ## for now assume there are always two lines between these
    question_heads <- question_divs + 2
    
    chapter[question_heads] <- mapply(FUN = function(x, y, chapter, chap_num) { 
        paste0(chapter[x], " ", chap_num, ".", y) }, 
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

## the ID attribute/anchor for margin figures ends up inside a comment block
## Here we look for these, and move the id into a 'name' attribute
## for the appropriate <img> tag
.retag_margin_figures <- function(chapter) {
    idx <- stringr::str_which(chapter, "<img.*<!--<span id")
    
    if(length(idx)) {
        id <- str_match(chapter[idx], pattern = "<!--<span (id=\"fig:[[:alnum:]-]+\")")[,2]
        chapter[idx] <- str_replace(chapter[idx], pattern = "<img ", paste0("<img ", id, " ")) %>%
            stringr::str_replace_all("<!--<span id=\"fig:.*</span>", "<!--")
    }
    
    return(chapter)
}

#' @importFrom xml2 read_html xml_find_all xml_add_sibling
.move_margin_table <- function(chapter) {
  
  chapter2 <- paste0(chapter, collapse = "\n")
  
  html <- xml2::read_html(chapter2)

  margin_tabs <- xml2::xml_find_all(html, 
                            xpath = "//table[contains(@class, 'margintab')]")
  
  for(i in seq_along(margin_tabs)) {
    ## paragraph immediately before, should be a caption
    caption <- xml_find_all(margin_tabs[[i]], xpath = "preceding-sibling::p[1]")[[1]]
    ## sibling 2 before - this may or may not be a code chunk
    code <- xml_find_all(margin_tabs[[i]], xpath = "preceding-sibling::*[2]")[[1]]
    if(grepl('<pre class="sourceCode', as.character(code))) {
      if(grepl('<caption>', as.character(caption))) {
        xml_add_sibling(code, caption, .where = "before", .copy = FALSE)
      }
      xml_add_sibling(code, margin_tabs[[i]], .where = "before", .copy = FALSE)
    } else if (grepl('<caption>', as.character(caption))) {
      xml_add_sibling(.x = margin_tabs[[i]], .value = caption, .where = "before", .copy = FALSE)
    }
  }
  
  chapter <- stringr::str_split(as.character(html), "\n")[[1]]
  return(chapter)
}

