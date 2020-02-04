context("gRbase misc")

##############################

UG <- ug(~a:b + b:c)

test_that("mcs()", {
    expect_equal(mcs(UG), c("a", "b", "c"))
})
