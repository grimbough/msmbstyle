
## modified version of tufte:::template_resources that allows
## us to specify the package where the resource can be found.
## rather than being hardwired to "tufte"
template_resources = function(name, package, ...) {
    system.file('rmarkdown', 'templates', name, 'resources', ..., package = package)
}

gsub_fixed = function(...) gsub(..., fixed = TRUE)

pandoc2.0 = function() rmarkdown::pandoc_available('2.0')

generate_id2 <- function() {
    f1 <- file.path(tempdir(), "solution_idx")
    
    id <- ifelse(file.exists(f1), readLines(f1), "1")
    id_new <- as.character(as.integer(id) + 1)
    writeLines(text = id_new, con = f1)

    return(id)
}

## javascript to be included for toggling solution visiblity
## TODO: There must be a more elegant way of including this, but
## it'll do for now.
toggle_script <- function() {
    
    return(
        "<script>
function toggle_visibility(id1, id2) {
var e = document.getElementById(id1);
var f = document.getElementById(id2);

e.style.display = ((e.style.display!='none') ? 'none' : 'block');

if(f.classList.contains('fa-plus-square')) {
    f.classList.add('fa-minus-square')
    f.classList.remove('fa-plus-square')
} else {
    f.classList.add('fa-plus-square')
    f.classList.remove('fa-minus-square')
}

}
</script>"
    )
}