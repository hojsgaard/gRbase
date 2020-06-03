context("gRbase tabX")

##############################

hec <- c(32, 53, 11, 50, 10, 25, 36, 66, 9, 34, 5, 29) 
dim(hec) <- c(2, 3, 2)
dimnames(hec) <- list(Hair = c("Black", "Brown"), 
                      Eye = c("Brown", "Blue", "Hazel"), 
                      Sex = c("Male", "Female"))

t12a <- tabMarg(hec, 1:2)
t12b <- tabMarg(hec, c("Hair", "Eye"))
t12c <- tabMarg(hec, ~Hair+Eye)

t23a <- tabMarg(hec, 2:3)


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


test_that("tabMarg()", {
    
    expect_equal(t12a, t12b)
    expect_equal(t12a, t12c)    
})

test_that("tabOp()", {
    t0 <- tableOp0(t12a, t23a, `*`)%>% tabPerm(~Hair+Eye+Sex)
    t1 <- tabMult(t12a, t23a) %>% tabPerm(~Hair+Eye+Sex)
    t2 <- tabOp(t12a, t23a, "*")
    t3 <- tableOp(t12a, t23a, "*") %>% tabPerm(~Hair+Eye+Sex)
    t4 <- tableOp(t12a, t23a, `*`) %>% tabPerm(~Hair+Eye+Sex)
    t5 <- t12a %a*% t23a

    expect_equal(t0, t1)
    expect_equal(t0, t2)
    expect_equal(t0, t3)
    expect_equal(t0, t4)
    expect_equal(t0, t5)    
})


