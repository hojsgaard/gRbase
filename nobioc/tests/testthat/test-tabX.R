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
t1a  <- tabMarg(hec, 1)


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

    t10 <- tableOp0(t12a, t1a, `*`)
    t11 <- tableOp0(t1a, t12a, `*`)
    t12 <- tabMult(t12a, t1a)
    t13 <- tabMult(t1a, t12a)

    expect_true(tabEqual(t10, t11))
    expect_true(tabEqual(t12, t13))
    expect_true(tabEqual(t10, t13))


    states <- list(asia=c("yes", "no"), tub=c("yes","no"))
    asia <- tabNew(~asia, levels=states, c(0.01, 0.99))
    tub  <- tabNew(~tub:asia, levels=states, c(0.05, 0.95, 0.01, 0.99))
    

    t0a=tableOp(tub, asia)
    t0b=tableOp(asia, tub)
    
    t1=tabMult(asia, tub)
    t2=tabMult(tub, asia)
    t5=tabOp(asia, tub, "*")
    t6=tabOp(tub, asia, "*")

    expect_equal(sum(t0a), 1)
    expect_equal(sum(t0b), 1)
    
    expect_equal(sum(t1), 1)
    expect_equal(sum(t2), 1)
    expect_equal(sum(t5), 1)
    expect_equal(sum(t6), 1)

    expect_true(tabEqual(t0a, t1))
    expect_true(tabEqual(t0a, t2))
    expect_true(tabEqual(t0a, t5))
    expect_true(tabEqual(t0a, t6))
})





