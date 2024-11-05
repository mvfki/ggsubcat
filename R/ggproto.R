#' @title Base ggproto classes for ggsubcat
#' @name ggsubcat-ggproto
#' @rdname ggsubcat-ggproto
#' @section Methodology:
#' As nicely described in the book \emph{GGPLOT2: Elegant Graphics for Data
#' Analysis}, the graph building theory comes with the data and definition of
#' selected aesthetics. After opting to add the desired geoms, ggplot2 system
#' maps the aesthetics to their corresponding scales, which project the data
#' values to the visual information (e.g. x/y to position, color-by variable to
#' color). Lastly, Guides are generated to help reading the graph so that people
#' can map the visual information back to data values (e.g. axes for reading x/y,
#' legend for knowing what each color represents).
#'
#' Following the streamline, each aesthetic leads to a scale and finally yields
#' a guide. For the sake of tidy visualization, guides coming from the same
#' aesthetics defined by the same data variables can be merged (e.g.
#' with \code{aes(colour = sample, shape = sample)}, you'll see colored point
#' each of a different shape in one legend).
#'
#' We designed ggsubcat to scale the sub-categorical information, made up by two
#' (aesthetic) variables, into one set of color and legend. The core trick
#' behind would be to allow the merging of two guides from different variables.
#' We first made the scale to be aware of two aesthetics that come in pair, and
#' have the mapping information cached in the scale. Then we modified the
#' hashables to be checked in the guides to avoid the previously described
#' blockage and turn the merging mechanism to check the sub-categorical mapping
#' previously cached, which makes sense.
NULL
