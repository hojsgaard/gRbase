context("gRbase tabX")

##############################

hec <- c(32, 53, 11, 50, 10, 25, 36, 66, 9, 34, 5, 29) 
dim(hec) <- c(2, 3, 2)
dimnames(hec) <- list(Hair = c("Black", "Brown"), 
                      Eye = c("Brown", "Blue", "Hazel"), 
                      Sex = c("Male", "Female"))


test_that("tabPerm()", {

    t0 <- tabPerm(hec, ~Eye:Sex:Hair) 
    t1 <- tabPerm(hec, c("Eye", "Sex", "Hair"))
    t2 <- tabPerm(hec, c(2, 3, 1)) 
    t3 <- tabPerm(hec, ~Ey:Se:Ha)  
    t4 <- tabPerm(hec, c("Ey", "Se", "Ha"))
    expect_equal(t1, t0)
    expect_equal(t2, t0)
    expect_equal(t3, t0)
    expect_equal(t4, t0)    
})
