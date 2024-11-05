#' @rdname ggsubcat-ggproto
#' @export
#' @usage NULL
#' @format NULL
#' @section Guides:
#' Object \code{GuideLegendSubcat} is provided for building sub-categorical
#' legends from \code{ScaleSubcat}. It inherits the original ggplot2 guide
#' \code{GuideLegend} and is modified with the following properties:
#' \itemize{
#'  \item{\code{params} - Fields of \code{customTitle}, \code{subcatMap} are
#'  added.}
#'  \item{\code{hashables} - Are modified to use \code{title}, \code{subcatMap},
#'  and \code{name} to compute the hash. Only in this way can we merge the
#'  internally resulted two guides for e.g. color and color.group with different
#'  aesthetic variables. As a result, this guide can never be merged with other
#'  guides.}
#' }
#'
#' The following methods are modified:
#' \itemize{
#'  \item{\code{train()} - Avoids auto title generation that the original
#'  \code{GuideLegend} does and in turn do not generate general title. And
#'  extracts the \code{subcatMap} from the scale.}
#'  \item{\code{setup_params()} - Computes number of breaks, nrow and ncol for
#'  each main category.}
#'  \item{\code{build_subtitles()} - A new method that overrides the place where
#'  the original \code{build_title()} is called. It generates title grobs for
#'  each main category.}
#'  \item{\code{build_decor()} - Build the key grobs only for one main category
#'  at a time.}
#'  \item{\code{draw()} - Splits the key table by main categories and build
#'  grob table for each main category, i.e. draw a "legend" for each main
#'  category, and combine them into one grob table.}
#'  \item{\code{measure_grobs()} and \code{arrange_layout()} - For the grob
#'  table of each main category, measures the width and height of the grob and
#'  arrange the element layout.}
#' }
GuideLegendSubcat <- ggproto(
    "GuideLegendSubcat",
    GuideLegend,
    params = list(
        title = waiver(),
        customTitle = waiver(),
        subcatMap = NULL,
        theme = NULL,

        # General
        override.aes = list(),
        nrow = NULL,
        ncol = NULL,
        reverse = FALSE,
        order = 0,

        name  = "legend_subcat",
        hash  = character(),
        position = NULL,
        direction = NULL
    ),
    hashables = exprs(title, subcatMap, name),
    train = function(self, params = self$params, scale, aesthetic = NULL,
                      ...)
    {
        # Keep the subcatMap, which will be used to check hash and build final
        # legend
        params$subcatMap <- scale$subcatMap
        params$aesthetic <- aesthetic %||% scale$aesthetics[1]
        params$key <- inject(self$extract_key(scale, !!!params))
        if (is.null(params$subcatMap)) {
            return(NULL)
        }
        params$decor <- inject(self$extract_decor(scale, !!!params))
        params <- self$extract_params(scale, params, ...)
        # The `title` param should still be considered as the overall main title
        # but it is now `extract_params`ed to be as the aes titles, which are
        # different and thus don't make sense to be checked for the hash and
        # to be shown as the main title.
        # `customTitle`, user-specified from `guide_legend_subcat()` is then
        # superseding it.
        params$title <- params$customTitle
        params$hash <- hash(lapply(unname(self$hashables), eval_tidy,
                                   data = params))
        params
    },
    setup_params = function(params) {
        params$direction <- arg_match0(
            params$direction,
            c("horizontal", "vertical"),
            arg_nm = "direction"
        )

        nrows <- ncols <- rep(list(NULL), length(params$subcatMap))
        for (i in seq_along(params$subcatMap)) {
            params$n_breaks[i] <- n_breaks_i <- length(params$subcatMap[[i]])
            params$n_key_layers <- length(params$decor) + 1
            if (!is.null(params$nrow) && !is.null(params$ncol)) {
                if (params$nrow[i] * params$ncol[i] < n_breaks_i) {
                    cli::cli_abort(paste0("{.arg nrow} * {.arg ncol} needs to be larger than the number of ",
                                          "breaks ({n_breaks_i})."))
                }
            }
            if (is.null(params$nrow) && is.null(params$ncol)) {
                if (params$direction == "horizontal") {
                    nrows[[i]] <- ceiling(n_breaks_i/5)
                }
                else {
                    ncols[[i]] <- ceiling(n_breaks_i/20)
                }
            }
            nrows[[i]] <- nrows[[i]] %||% ceiling(n_breaks_i/ncols[[i]])
            ncols[[i]] <- ncols[[i]] %||% ceiling(n_breaks_i/nrows[[i]])
        }
        params$nrow <- unlist(nrows)
        params$ncol <- unlist(ncols)
        params
    },
    build_subtitles = function(label, elements, params)
    {
        lapply(label, function(l) {
            ggname(
                "guide.title",
                element_grob(
                    elements$title,
                    label = l,
                    margin_x = TRUE,
                    margin_y = TRUE
                )
            )
        })

    },
    build_decor = function(decor, grobs, elements, params, group_i)
    {
        key_size <- c(elements$width_cm, elements$height_cm) * 10
        draw <- function(i) {
            bg <- elements$key
            keys <- lapply(decor, function(g) {
                data <- vec_slice(g$data, i)
                if (data$.draw %||% TRUE) {
                    key <- g$draw_key(data, g$params, key_size)
                    set_key_size(key, data$linewidth, data$size,
                                 key_size/10)
                }
                else {
                    zeroGrob()
                }
            })
            c(list(bg), keys)
        }
        unlist(lapply(seq_len(params$n_breaks[group_i]), draw), FALSE)
    },
    draw = function(self, theme, position = NULL, direction = NULL,
                    params = self$params)
    {
        params <- replace_null(params, position = position, direction = direction)
        params <- self$setup_params(params)
        key <- params$key
        elems <- self$setup_elements(params, self$elements, theme)
        elems <- self$override_elements(params, elems, theme)
        if (prod(dim(key)) == 0) {
            out <- self$draw_early_exit(params, elems)
            return(out)
        }
        gt.list <- list()
        for (i in seq_along(params$subcatMap)) {
            subtitle <- names(params$subcatMap)[i]
            members <- params$subcatMap[[i]]
            key.sub <- key[key$.value %in% members,]
            if (empty(key.sub)) {
                next
            }

            decor.sub <- params$decor
            decor.sub[[1]]$data <- decor.sub[[1]]$data[key$.value %in% members,]
            grobs <- list(title = self$build_title(subtitle, elems, params),
                          ticks = self$build_ticks(key.sub, elems, params))
            if (params$draw_label %||% TRUE) {
                grobs$labels <- self$build_labels(key.sub, elems, params)
            }
            else {
                grobs$labels <- list(zeroGrob())
            }

            grobs$decor <- self$build_decor(decor.sub, grobs, elems,
                                            params, i)
            sizes <- self$measure_grobs(grobs, params, elems, i)
            layout <- self$arrange_layout(key.sub, sizes, params, elems, i)
            gt.list[[i]] <- self$assemble_drawing(grobs, layout, sizes, params, elems)
        }
        switch(params$direction,
            horizontal = Reduce(cbind, gt.list),
            vertical = Reduce(rbind, gt.list)
        )
    },
    measure_grobs = function(grobs, params, elements, group_i)
    {
        byrow <- elements$byrow %||% FALSE
        n_breaks <- params$n_breaks[group_i] %||% 1L
        dim <- c(params$nrow[group_i] %||% 1L, params$ncol[group_i] %||% 1L)
        sizes <- params$sizes %||%
            measure_legend_keys(
                grobs$decor,
                n = n_breaks,
                dim = dim,
                byrow = byrow,
                default_width = elements$width_cm,
                default_height = elements$height_cm
            )
        widths <- sizes$widths
        heights <- sizes$heights
        zeroes <- rep(0, prod(dim) - n_breaks)
        label_widths <- apply(matrix(c(width_cm(grobs$labels), zeroes),
                                     nrow = dim[1], ncol = dim[2], byrow = byrow), 2, max)
        label_heights <- apply(matrix(c(height_cm(grobs$labels),
                                        zeroes), nrow = dim[1], ncol = dim[2], byrow = byrow),
                               1, max)
        hgap <- elements$spacing_x %||% 0
        widths <- switch(elements$text_position, left = list(label_widths,
                                                             widths, hgap), right = list(widths, label_widths, hgap),
                         list(pmax(label_widths, widths), hgap))
        widths <- head(vec_interleave(!!!widths), -1)
        vgap <- elements$spacing_y %||% 0
        heights <- switch(elements$text_position, top = list(label_heights,
                                                             heights, vgap), bottom = list(heights, label_heights,
                                                                                           vgap), list(pmax(label_heights, heights), vgap))
        heights <- head(vec_interleave(!!!heights), -1)
        list(widths = widths, heights = heights)
    },
    arrange_layout = function(key, sizes, params, elements, group_i)
    {
        break_seq <- seq_len(params$n_breaks[group_i] %||% 1L)
        dim <- c(params$nrow[group_i] %||% 1L, params$ncol[group_i] %||% 1L)
        if (elements$byrow %||% FALSE) {
            row <- ceiling(break_seq/dim[2L])
            col <- (break_seq - 1L) %% dim[2L] + 1L
        }
        else {
            row <- (break_seq - 1L) %% dim[1L] + 1L
            col <- ceiling(break_seq/dim[1L])
        }
        key_row <- row * 2 - 1
        key_col <- col * 2 - 1
        position <- elements$text_position
        key_row <- key_row + switch(position, top = row, bottom = row -
                                        1, 0)
        lab_row <- key_row + switch(position, top = -1, bottom = 1,
                                    0)
        key_col <- key_col + switch(position, left = col, right = col -
                                        1, 0)
        lab_col <- key_col + switch(position, left = -1, right = 1,
                                    0)
        data_frame0(key_row = key_row, key_col = key_col, label_row = lab_row,
                    label_col = lab_col)
    }
)

#' Sub-categorical legend guide
#' @description
#' Legend type guide shows key (i.e. geoms) mapped onto values. Different than
#' a ggplot2 original legend guide, keys are split by parent categories and then
#' parent category names are added as sub-titles. Integration with other scales
#' is not possible.
#' @param title A character string or expression indicating an overall title of
#' guide. If \code{NULL}, the title is not shown. By default
#' (\code{\link[ggplot2]{waiver}()}), the name of the scale object or the name
#' specified in \code{\link[ggplot2]{labs}()} is used for the title.
#' @param theme A \code{\link[ggplot2]{theme}} object to style the guide
#' individually or differently from the plot's theme settings. The \code{theme}
#' argument in the guide overrides, and is combined with, the plot's theme.
#' @param position A character string indicating where the legend should be
#' placed relative to the plot panels.
#' @param direction A character string indicating the direction of the guide.
#' One of \code{"horizontal"} or \code{"vertical"}.
#' @param override.aes A list specifying aesthetic parameters of legend key. See
#' details and examples. TODO, anyway to avoid Duplicated `override.aes`
#' warning? Since at graph building time, two copies of the same guide are
#' generated for e.g. `color` and `color.group` and then merged.
#' @param nrow,ncol The desired number of rows and column of legends
#' respectively. TODO, this applies to inidividual group for now but ideally we
#' need a better scheme for overall arranging of all groups.
#' @param reverse logical. If \code{TRUE} the order of legends is reversed.
#' TODO, make \code{reverse.inner} and \code{reverse.outer} separetely.
#' @param order Positive integer less than 99 that specifies the order of this
#' guide among multiple guides. This controls the order in which multiple guides
#' are displayed, not the contents of the guide itself. If 0 (default), the
#' order is determined by a secret algorithm.
#' @param ... Ignored
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(subcatPoints, aes(x, y, color = sub, color.group = main)) +
#'     geom_point() +
#'     scale_color_subcat(guide = guide_legend_subcat(
#'         position = "bottom"
#'     ))
guide_legend_subcat <- function(
        title = waiver(),
        theme = NULL,
        position = NULL,
        direction = NULL,
        override.aes = list(),
        nrow = NULL,
        ncol = NULL,
        reverse = FALSE,
        order = 0,
        ...
) {
    theme <- deprecated_guide_args(theme, ...)
    if (!is.null(position)) {
        position <- arg_match0(position, c(.trbl, "inside"))
    }

    new_guide(
        customTitle = title, theme = theme, direction = direction,
        override.aes = rename_aes(override.aes), nrow = nrow,
        ncol = ncol, reverse = reverse, order = order, position = position,
        available_aes = "any", name = "legend_subcat", super = GuideLegendSubcat
    )
}



######### Other necessary helper functions that ggplot2 doesn't export #########

rename_aes <- function(x) {
    names(x) <- standardise_aes_names(names(x))
    duplicated_names <- names(x)[duplicated(names(x))]
    if (length(duplicated_names) > 0L) {
        cli::cli_warn("Duplicated aesthetics after name standardisation: {.field {unique0(duplicated_names)}}")
    }
    x
}

deprecated_guide_args <- function(
        theme = NULL, title.position = NULL, title.theme = NULL,
        title.hjust = NULL, title.vjust = NULL, label = NULL, label.position = NULL,
        label.theme = NULL, label.hjust = NULL, label.vjust = NULL,
        keywidth = NULL, keyheight = NULL, barwidth = NULL, barheight = NULL,
        byrow = NULL, frame.colour = NULL, frame.linewidth = NULL,
        frame.linetype = NULL, ticks = NULL, ticks.colour = NULL,
        ticks.linewidth = NULL, axis = NULL, axis.colour = NULL,
        axis.linewidth = NULL, axis.arrow = NULL, default.unit = "line",
        ..., .call = caller_call()
) {
    args <- names(formals(deprecated_guide_args))
    args <- setdiff(args, c("theme", "default.unit", "...", ".call"))
    vals <- compact(mget(args, current_env()))
    if (length(vals) == 0) {
        return(theme)
    }
    fun_name <- call_name(.call)
    replacement <- paste0(fun_name, "(theme)")
    for (arg_name in names(vals)) {
        deprecate_soft0(when = "3.5.0", what = paste0(fun_name,
                                                      "(", arg_name, ")"), with = replacement)
    }
    def_unit <- function(x) {
        if (is.null(x) || inherits(x, "unit")) {
            return(x)
        }
        unit(x, default.unit)
    }
    theme <- theme %||% theme()
    theme <- replace_null(theme, legend.title.position = title.position,
                          legend.text.position = label.position, legend.byrow = byrow,
                          legend.key.width = def_unit(keywidth %||% barwidth),
                          legend.key.height = def_unit(keyheight %||% barheight))
    if (isFALSE(label)) {
        label.theme <- element_blank()
    }
    else if (!is.null(label.theme %||% label.hjust %||% label.vjust)) {
        label.theme <- label.theme %||% element_text()
        label.theme <- replace_null(label.theme, hjust = label.hjust %||%
                                        label.theme$hjust, vjust = label.vjust %||% label.theme$vjust)
    }
    theme$legend.text <- theme$legend.text %||% label.theme
    if (!is.null(title.hjust %||% title.vjust)) {
        title.theme <- title.theme %||% element_text()
        title.theme <- replace_null(title.theme, hjust = title.hjust %||%
                                        title.theme$hjust, vjust = title.vjust %||% title.theme$vjust)
    }
    theme$legend.title <- theme$legend.title %||% title.theme
    if (!is.null(frame.colour %||% frame.linewidth %||% frame.linetype)) {
        frame <- theme$legend.frame %||% element_rect(colour = frame.colour,
                                                      linewidth = frame.linewidth, linetype = frame.linetype)
        theme$legend.frame <- theme$legend.frame %||% frame
    }
    if (isFALSE(ticks)) {
        ticks <- element_blank()
    }
    else if (!is.null(ticks.colour %||% ticks.linewidth)) {
        ticks <- element_line(colour = ticks.colour, linewidth = ticks.linewidth)
        theme$legend.ticks <- theme$legend.ticks %||% ticks
    }
    if (isFALSE(axis)) {
        axis <- element_blank()
    }
    else if (!is.null(axis.colour %||% axis.linewidth %||% axis.arrow)) {
        axis <- element_line(colour = axis.colour, linewidth = axis.linewidth,
                             arrow = axis.arrow)
        theme$legend.axis.line <- theme$legend.axis.line %||%
            axis
    }
    theme <- compact(theme)
    if (!is.theme(theme)) {
        theme <- inject(theme(!!!theme))
    }
    theme
}

compact <- function(x) {
    null <- vapply(x, is.null, logical(1))
    x[!null]
}

deprecate_soft0 <- function(..., user_env = NULL) {
    user_env <- user_env %||% getOption("ggplot2_plot_env") %||%
        caller_env(2)
    lifecycle::deprecate_soft(..., user_env = user_env)
}

replace_null <- function(obj, ..., env = caller_env()) {
    dots <- enexprs(...)
    nms <- names(dots)
    nms <- nms[vapply(obj[nms], is.null, logical(1))]
    obj[nms] <- inject(list(!!!dots[nms]), env = env)
    obj
}
