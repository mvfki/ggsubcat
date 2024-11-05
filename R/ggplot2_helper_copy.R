is.waive <- function(x) inherits(x, "waiver")

empty <- function(df) {
    is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is.waive(df)
}

is.discrete <- function(x) {
    is.factor(x) || is.character(x) || is.logical(x)
}

.trbl <- c("top", "right", "bottom", "left")

is.zero <- function (x) is.null(x) || inherits(x, "zeroGrob")

ggname <- function(prefix, grob)
{
    grob$name <- grobName(grob, prefix)
    grob
}


set_key_size <- function(key, linewidth = NULL, size = NULL, default = NULL)
{
    if (!is.null(attr(key, "width")) && !is.null(attr(key, "height"))) {
        return(key)
    }
    if (!is.null(size) || !is.null(linewidth)) {
        size <- size %||% 0
        linewidth <- linewidth %||% 0
        size <- if (is.na(size)[1])
            0
        else size[1]
        linewidth <- if (is.na(linewidth)[1])
            0
        else linewidth[1]
        size <- (size + linewidth)/10
    }
    else {
        size <- NULL
    }
    attr(key, "width") <- attr(key, "width", TRUE) %||% size %||%
        default[1]
    attr(key, "height") <- attr(key, "height", TRUE) %||% size %||%
        default[2]
    key
}

measure_legend_keys <- function(keys, n, dim, byrow = FALSE, default_width = 1, default_height = 1)
{
    if (is.null(keys)) {
        ans <- list(widths = NULL, heights = NULL)
        return(ans)
    }
    padding_zeroes <- rep(0, prod(dim) - n)
    widths <- c(get_key_size(keys, "width", n), padding_zeroes)
    heights <- c(get_key_size(keys, "height", n), padding_zeroes)
    widths <- matrix(widths, nrow = dim[1], ncol = dim[2], byrow = byrow)
    heights <- matrix(heights, nrow = dim[1], ncol = dim[2],
                      byrow = byrow)
    list(widths = pmax(default_width, apply(widths, 2, max)),
         heights = pmax(default_height, apply(heights, 1, max)))
}

get_key_size <- function(keys, which = "width", n)
{
    size <- lapply(keys, attr, which = which)
    size[lengths(size) != 1] <- 0
    size <- matrix(unlist(size), ncol = n)
    apply(size, 2, max)
}

width_cm <- function(x)
{
    if (is.grob(x)) {
        convertWidth(grobWidth(x), "cm", TRUE)
    }
    else if (is.unit(x)) {
        convertWidth(x, "cm", TRUE)
    }
    else if (is.list(x)) {
        vapply(x, width_cm, numeric(1))
    }
    else {
        cli::cli_abort("Don't know how to get width of {.cls {class(x)}} object")
    }
}

height_cm <- function(x)
{
    if (is.grob(x)) {
        convertHeight(grobHeight(x), "cm", TRUE)
    }
    else if (is.unit(x)) {
        convertHeight(x, "cm", TRUE)
    }
    else if (is.list(x)) {
        vapply(x, height_cm, numeric(1))
    }
    else {
        cli::cli_abort("Don't know how to get height of {.cls {class(x)}} object")
    }
}

data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")
