test_that("normal working", {
    expect_no_error({
        p <- ggplot(subcatPoints, aes(x, y, color = sub, color.group = main)) +
            geom_point() +
            scale_color_subcat()
    })
    expect_is(p, "gg")
    expect_no_error({gbulid <- ggplot_build(p)})

    expect_no_error({
        p <- ggplot(subcatPoints, aes(x, y, colour = sub, colour.group = main)) +
            geom_point() +
            scale_colour_subcat()
    })
    expect_is(p, "gg")

    expect_no_error({
        p <- ggplot(subcatPoints, aes(x, y, fill = sub, fill.group = main)) +
            geom_bar(stat = "identity") +
            scale_fill_subcat()
    })
    expect_is(p, "gg")
})

test_that("proper scale/guide information", {
    p <- ggplot(subcatPoints, aes(x, y, color = sub, color.group = main)) +
        geom_point() +
        scale_color_subcat()
    expect_equal(length(p$scales$scales), 1)
    expect_true(inherits(p$scales$scales[[1]], "ScaleSubcat"))
    expect_true(all.equal(p$scales$scales[[1]]$aesthetics, c("colour", "colour.group")))
})
