# Checks whether y is sub-categorical upon x
# e.g. valid:
#      x =  a,  a,  b,  b,  c,  c
#      y = a1, a2, b1, b2, c1, c2
# invalid:
#      x =  a,  a,  b,  b,  c,  c
#      y = a1, a2, b1, c1, a1, a2
.mapSubcat <- function(
        main,
        sub
) {
    if (length(main) != length(sub)) {
        cli::cli_abort(
            c(x = "{.var main} and {.var sub} must have the same length",
              i = "{.var main} has {length(main)} element{?s} and {.var sub} has {length(sub)} element{?s}")
        )
    }
    # Check that each category from `sub` only map to one category in `main`
    tab <- table(main, sub)
    cs <- colSums(tab > 0)
    if (any(cs > 1)) {
        bad <- names(cs)[cs > 1]
        cli::cli_abort(
            c(
                x = "Invalid subcategorical mapping.",
                i = "{length(bad)} sub-categor{?y/ies} {?is/are} mapped to more than one main categories: {.val {bad}}."
            )
        )
    }
    map <- rep(list(NULL), nrow(tab))
    names(map) <- rownames(tab)

    tabdf <- as.data.frame(tab)
    tabdf <- tabdf[tabdf$Freq > 0, 1:2]
    rownames(tabdf) <- NULL


    for (i in seq_len(nrow(tabdf))) {
        key <- as.character(tabdf[i, 1])
        val <- as.character(tabdf[i, 2])
        map[[key]] <- c(map[[key]], val)
    }
    return(map)
}

# Make sub category colors from given main category color.
# e.g. When color given `col` is some blue, and three sub-cats are mapped,
# return three shades of blue.
.shade <- function(
        col,
        n,
        lMin = 20,
        lMax = 80,
        cMin = NULL,
        cMax = NULL
) {
    if (n == 1) return(col)
    hcl <- farver::decode_colour(col, to = "hcl")
    l <- hcl[1, 3]
    if (l > (lMax + lMin) / 2) {
        lMax <- l
        l_all <- seq(lMax, lMin, by = -20)
        if (length(l_all) >= n) l_all <- l_all[seq_len(n)]
        else l_all <- seq(lMax, lMin, length.out = n)
    } else {
        lMin <- l
        l_all <- seq(lMin, lMax, by = 20)
        if (length(l_all) >= n) l_all <- l_all[seq_len(n)]
        else l_all <- seq(lMax, lMin, length.out = n)
    }

    if (is.null(cMin) && is.null(cMax)) c_all <- NULL
    else if (is.null(cMin) && !is.null(cMax)) c_all <- cMax
    else if (!is.null(cMin) && is.null(cMax)) c_all <- cMin
    else c_all <- seq(cMax, cMin, length.out = n)

    scales::col2hcl(colour = rep(col, n), c = c_all, l = l_all)
}


# Make sub category colors from given main category color.
# e.g. When color given `col` is some blue, and three sub-cats are mapped,
# return three shades of blue.
pal_shade <- function(
        col,
        lMin = 20,
        lMax = 80,
        cMin = NULL,
        cMax = NULL
) {
    return_func <- function(n) {

        if (n == 1) return(col)
        hcl <- farver::decode_colour(col, to = "hcl")
        l <- hcl[1, 3]
        if (l > (lMax + lMin) / 2) {
            lMax <- l
            l_all <- seq(lMax, lMin, by = -20)
            if (length(l_all) >= n) l_all <- l_all[seq_len(n)]
            else l_all <- seq(lMax, lMin, length.out = n)
        } else {
            lMin <- l
            l_all <- seq(lMin, lMax, by = 20)
            if (length(l_all) >= n) l_all <- l_all[seq_len(n)]
            else l_all <- seq(lMax, lMin, length.out = n)
        }

        if (is.null(cMin) && is.null(cMax)) c_all <- NULL
        else if (is.null(cMin) && !is.null(cMax)) c_all <- cMax
        else if (!is.null(cMin) && is.null(cMax)) c_all <- cMin
        else c_all <- seq(cMax, cMin, length.out = n)

        scales::col2hcl(colour = rep(col, n), c = c_all, l = l_all)
    }
    return(return_func)
}


.checkMainColor <- function(main, mainColors = NULL) {
    mainCats <- if (is.factor(main)) levels(main) else unique(main)
    n <- length(mainCats)
    if (is.null(mainColors)) {
        mainColors <- scales::hue_pal()(n)
        names(mainColors) <- mainCats
        return(mainColors)
    }
    if (length(mainColors) < n) {
        cli::cli_abort(
            c(
                x = "Insufficient main colors specified",
                i = "{.var main} has {n} categor{?y/ies} but only {length(mainColors)} color{?s} specified."
            )
        )
    }
    if (is.list(mainColors)) {
        if (!is.null(names(mainColors))) {
            if (!all(mainCats %in% names(mainColors))) {
                cli::cli_abort(
                    c(
                        x = "Named list input of {.var mainColors} does not contain all categories in {.var main}.",
                        i = "Missing: {.val {setdiff(mainCats, names(mainColors))}}."
                    )
                )
            }
            mainColors <- mainColors[mainCats]
        } else {
            mainColors <- mainColors[seq_len(n)]
            names(mainColors) <- mainCats
        }
        eachPass <- sapply(mainColors, function(col) length(col) == 1 & is.character(col))
        if (!all(eachPass)) {
            cli::cli_abort(
                c(
                    x = "List input of {.var mainColors} must contain a single color for each element.",
                    i = "Found {sum(eachPass)} invalid element{?s}"
                )
            )
        }
        mainColors <- unlist(mainColors)
    } else {
        if (!is.null(names(mainColors))) {
            if (!all(mainCats %in% names(mainColors))) {
                cli::cli_abort(
                    c(
                        x = "Named vector input of {.var mainColors} does not contain all categories in {.var main}.",
                        i = "Missing: {.val {setdiff(mainCats, names(mainColors))}}."
                    )
                )
            }
            mainColors <- mainColors[mainCats]
        } else {
            mainColors <- mainColors[seq_len(n)]
            names(mainColors) <- mainCats
        }
    }
    return(mainColors)
}

#' Generate colors for sub-categories
#' @description
#' This function first maps sub-categories to corresponding main categories,
#' determines the colors for main categories, and then generates shaded colors
#' for sub-categories belonging to each main category.
#'
#' @details
#' The shading process mainly works by tuning the luminance of a given color
#' in HCL space. The range of acceptable luminance is determined by \code{lMin}
#' and \code{lMax}. Since the given color has its luminance, the function
#' decides whether to increase or decrease the luminance based on the given
#' range. If the luminance of the given color is larger than the mean of the
#' range, the function will decrease the luminance from the given color to
#' \code{lMin}. Otherwise, it will increase the luminance from the given color
#' to \code{lMax}. It by default increases or decreases the luminance by 20.
#' When more colors are needed, the function will evenly space the luminance.
#'
#' The chroma can be tuned in the meantime. If none of \code{cMin} or
#' \code{cMax} is set (all \code{NULL}), the chroma of the given color will be
#' retained. If only one of \code{cMin} or \code{cMax} is given, the function
#' will use the given value for the colors of all sub-categories. If both are
#' given, the function will evenly space the chroma between the two values, from
#' max to min.
#' @param main A factor/vector representing the variable with the main
#' categories
#' @param sub A factor/vector representing the variable with the sub-categories.
#' @param mainColors A named/unnamed vector/list of colors for the main
#' categories. Colors should be presented with a string color code that R can
#' understand. For example \code{"red"} or \code{"#FF0000"}. Default \code{NULL}
#' use evenly spaced hues (ggplot2 default discrete colors).
#' @param lMin,lMax Minimum and maximum luminance for the shaded colors, must be
#' between \[0, 100\] with min less then max. Default is \code{20} and \code{80}.
#' @param cMin,cMax Minimum and maximum chroma for the HCL color space, must be
#' between \[0, 100\] with min less than max. Default is \code{NULL} for both.
#' @return list object. Each element is named by the categories in \code{main}.
#' Each element is a named vector of colors for the sub-categories mapped to the
#' corresponding main category.
#' @export
#' @examples
#' makeSubcatCol(
#'     main = rep(letters[1:3], 3),
#'     sub = paste0(rep(letters[1:3], 3), 1:9)
#' )
makeSubcatCol <- function(
        main,
        sub,
        mainColors = NULL,
        lMin = 20,
        lMax = 80,
        cMin = NULL,
        cMax = NULL
) {
    map <- .mapSubcat(main, sub)
    mainColors <- .checkMainColor(main, mainColors)
    mainCats <- names(mainColors)
    subColors <- lapply(mainCats, function(nm) {
        cols <- .shade(
            col = mainColors[nm], n = length(map[[nm]]),
            lMin = lMin, lMax = lMax,
            cMin = cMin, cMax = cMax
        )
        names(cols) <- map[[nm]]
        return(cols)
    })
    names(subColors) <- mainCats
    # TODO IMPORTANT
    # Check that out of colors for all sub-categories, no color is repeated.
    # If any repeats, "jitter" the colors.
    return(subColors)
}
