#' Sub-categorical colour scales
#' @description
#' The sub-categorical colour scale. Work with one of the pairs of aesthetics
#' specified: \code{colour} and \code{colour.group} or \code{fill} and
#' \code{fill.group}. A level in the variable that \code{colour} specifies must
#' map to one and only one level in the variable that \code{colour.group}
#' specifies. The same rule applies to the pair \code{fill} and
#' \code{fill.group}.
#' @param lMin,lMax Minimum and maximum luminance for the shaded colors, must be
#' between \[0, 100\] with min less then max. Default is \code{20} and \code{80}.
#' @param cMin,cMax Minimum and maximum chroma for the HCL color space, must be
#' between \[0, 100\] with min less than max. Default is \code{NULL} for both.
#' @param ... other arguments that constructs a discrete scale. TODO.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(subcatPoints, aes(x, y, color = sub, color.group = main)) +
#'     geom_point() +
#'     scale_colour_subcat()
#'
#' ggplot(subcatPoints, aes(sub, y, fill = sub, fill.group = main)) +
#'    geom_bar(stat = "identity") +
#'    scale_fill_subcat()
scale_colour_subcat <- function(
        lMin = 20,
        lMax = 80,
        cMin = NULL,
        cMax = NULL,
        ...
) {
    subcat_scale(
        aesthetics = c("colour", "colour.group"),
        lMin = lMin,
        lMax = lMax,
        cMin = cMin,
        cMax = cMax,
        ...
    )
}

#' @export
#' @rdname scale_colour_subcat
#' @usage NULL
scale_color_subcat <- scale_colour_subcat

#' @export
#' @rdname scale_colour_subcat
scale_fill_subcat <- function(
        lMin = lMin,
        lMax = lMax,
        cMin = cMin,
        cMax = cMax,
        ...
) {
    subcat_scale(
        aesthetics = c("fill", "fill.group"),
        ...
    )
}

#' @rdname ggsubcat-ggproto
#' @export
#' @format NULL
#' @usage NULL
#' @section Scales:
#' Object \code{ScaleSubcat} is provided for sub-categorical color scaling.
#'
#' It inherits from ggplot2's original scale \code{ScaleDiscrete} and is
#' modified with the following properties:
#' \itemize{
#'  \item{params - A list of additional parameters and caches to be used for
#'  forming the palette.}
#' }
#'
#' And also modified with the following methods:
#' \itemize{
#'  \item{train_df() - Checks the pair of aesthetics (e.g. \code{colour} and
#'  \code{colour.group}), identifies the correct sub-categorical mapping, and
#'  predefines the palette for the categories in e.g. \code{colour.group}.}
#'  \item{palette() - Uses the pre-defined parameters to form the colors being
#'  used.}
#' }
ScaleSubcat <- ggproto(
    "ScaleSubcat",
    ScaleDiscrete,
    train_df = function(self, df) {
        if (empty(df))
            return()
        aesthetics <- intersect(self$aesthetics, names(df))
        # By sorting, we hope to get "*.group" at last
        aesthetics <- sort(aesthetics)
        if (length(aesthetics) == 0) {
            cli::cli_abort(c(
                "No required aesthetics specified for sub-categorical scale.",
                i = "Use either {.field colour} and {.field colour.group} or {.field fill} and {.field fill.group}."
            ))
        } else if (length(aesthetics) == 1) {
            if (endsWith(aesthetics, ".group")) {
                cli::cli_abort(c(
                    "Insufficient aesthetics for sub-categorical scale.",
                    i = "Missing {.field {gsub('.group', '', aesthetics)}}."
                ))
            } else {
                cli::cli_abort(c(
                    "Insufficient aesthetics for sub-categorical scale.",
                    i = "Missing {.field {aesthetics}.group}."
                ))
            }
        }
        if (aesthetics[2] != paste0(aesthetics[1], ".group")) {
            cli::cli_abort("Error identifying the pair of aesthetics. Should be `colour` and `colour.group` or `fill` and `fill.group`.",
                           i = "Got {.val {aesthetics}}.")
        }
        mapList <- .mapSubcat(df[[aesthetics[2]]], df[[aesthetics[1]]])
        # One of the most important trick, to record the mapping relationship in
        # the ggproto OO system
        self$subcatMap <- mapList
        # Make proper levels that reflect the parent order and the children
        # order
        df[[aesthetics[1]]] <- factor(
            df[[aesthetics[1]]],
            levels = unlist(
                mapList,
                use.names = FALSE
            )
        )
        self$train(df[[aesthetics[1]]])
        invisible()
    },
    palette = function(self, n) {
        # The most important trick, to retrieve the mapping relationship in
        # the ggproto OO system with `self`
        mapList <- self$subcatMap
        main <- rep(names(mapList), lengths(mapList))
        mainColors <- self$params$mainPalette(length(mapList))
        mainColors <- .checkMainColor(main, mainColors)
        mainCats <- names(mainColors)
        subColors <- lapply(mainCats, function(nm) {
            cols <- .shade(
                col = mainColors[nm], n = length(mapList[[nm]]),
                lMin = self$params$lMin, lMax = self$params$lMax,
                cMin = self$params$cMin, cMax = self$params$cMax
            )
            names(cols) <- mapList[[nm]]
            return(cols)
        })
        # names(subColors) <- mainCats
        # TODO IMPORTANT
        # Check that out of colors for all sub-categories, no color is repeated.
        # If any repeats, "jitter" the colors.
        return(unname(unlist(subColors)))
    },
    params = list(
        lMin = 20,
        lMax = 80,
        cMin = NULL,
        cMax = NULL,
        mainPalette = NULL
    )
)

subcat_scale <- function(
        aesthetics,
        lMin = 20,
        lMax = 80,
        cMin = NULL,
        cMax = NULL,
        name = waiver(),
        breaks = waiver(),
        labels = waiver(),
        limits = NULL,
        expand = waiver(),
        na.translate = TRUE,
        na.value = NA,
        drop = TRUE,
        guide = "legend_subcat",
        position = "left",
        call = caller_call()
) {
    mainPalette <- scales::hue_pal()
    ggproto(
        NULL,
        ScaleSubcat,
        call = call,
        aesthetics = aesthetics,
        range = DiscreteRange$new(),
        limits = limits,
        na.value = na.value,
        na.translate = na.translate,
        expand = expand,
        name = name,
        breaks = breaks,
        labels = labels,
        drop = drop,
        guide = guide,
        # guide = guide_sublegends,
        position = position,
        params = list(
            lMin = lMin,
            lMax = lMax,
            cMin = cMin,
            cMax = cMax,
            mainPalette = mainPalette
        )
    )
}
