# **msmbstyle**

The **msmbstyle** package provides an alternative design for HTML books produced using the R package [bookdown](http://www.bookdown.org).  The layout was developed to closely match an existing publication produced using Sweave and LaTeX.  The general structure is heavily influenced by the handout design of Edward Tufte, with figures and annotations appearing in the margin alongside the main text, and much of the document conversion/creation is actually carried out by the [**tufte** package](https://github.com/rstudio/tufte).  **msmbstyle** provides some additional modifications to the overall layout of the book.

You can view a simple example of the HTML produced by **msmbstyle** at http://www-huber.embl.de/users/msmith/msmbstyle/, and the R Markdown documents used to generate this are present in the *tests* folder of this package

A example of a complete book generated using **msmbstyle** can be found at [Modern Statistics for Modern Biology](http://www-huber.embl.de/msmb/) by S.&#xA0;Holmes & W.&#xA0;Huber.

## **msmbstyle** vs **tufte** styling

A (probably incomplete) list of the layout differences between an HTML book produced by **msmbstyle** and the default options in **tufte**:

- Book title and author names listed in a header bar at the top of each page.
- Document navigation provided by drop-down menu in the top right of each page.
    - One entry per chapter.
    - The current chapter has sections listed too.
- Sans serif font (current using [Source Sans Pro](https://fonts.google.com/specimen/Source+Sans+Pro))
- Keeps default width of code blocks when vertically aligned with margin figures.
- Provides CSS support to place tables in the margin along with figures and notes.
- Defines additional document sections (currently Questions & Solutions) that can be referenced in text, and have their visibility toggled within the final document.
