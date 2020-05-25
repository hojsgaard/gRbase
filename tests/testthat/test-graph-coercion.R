context("test-graph-coercion.R")

## #########################################################

## Coercion of adjacency lists to ft-matrix and tf-matrix

ll <-
    list("1"=as.character(2:3),
         "2"=as.character(3:4),
         "3"=character(0),
         "4"=character(0))

mat4 <- 
    structure(c(0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0),
              .Dim = c(4L, 4L),
              .Dimnames = list(c("1", "2", "3", "4"),
                               c("1", "2", "3", "4")))

mat0 <-
    structure(numeric(0), .Dim = c(0L, 0L))


test_that("adjList2matrix", {
    expect_equal(adjList2matrix__(ll),
                 mat4)
    expect_equal(adjList2matrix__(list()),
                 mat0)
    
    expect_equal(adjList2dgCMatrix__(ll),
                 as(mat4, "dgCMatrix"))
    expect_equal(adjList2dgCMatrix__(list()),
                 as(mat0, "dgCMatrix"))
})

test_that("adjList2ftList", {
    expect_equal(adjList2tfList__(ll),
                 list(c("2", "1"), c("3", "1"), c("3", "2"), c("4", "2")))
    
    expect_equal(adjList2tfList__(list()),
                 list())
    
    expect_equal(adjList2ftList__(ll),
                 list(c("1", "2"), c("1", "3"), c("2", "3"), c("2", "4")))
    
    expect_equal(adjList2ftList__(list()),
                 list())
})


## #########################################################

ss <- list(as.character(1:3));
vn <- unique(unlist(ss))

## Coercion - list (defining undirected graph) to matrix

dval3 <-
    structure(c(0, 1, 1, 1, 0, 1, 1, 1, 0), .Dim = c(3L, 3L),
              .Dimnames = list(c("1", "2", "3"), c("1", "2", "3")))

dval0 <-
    structure(numeric(0), .Dim = c(0L, 0L))

sval3 <- as(dval3, "dgCMatrix")
sval0 <- as(dval0, "dgCMatrix")

test_that("ugList2matrix", {

    a1 <- ugList2dgCMatrix__(ss, vn)
    a2 <- ugList2dgCMatrix__(ss, NULL)
    a3 <- ugList2dgCMatrix__(ss, c(1,2,3))
    a4 <- ugList2dgCMatrix__(NULL,NULL)
    a5 <- ugList2dgCMatrix__(NULL,c(1,2,3))
    
    b1 <- ugList2matrix__(ss, vn)
    b2 <- ugList2matrix__(ss, NULL)
    b3 <- ugList2matrix__(ss, c(1,2,3))
    b4 <- ugList2matrix__(NULL,NULL)
    b5 <- ugList2matrix__(NULL,c(1,2,3))
    
    expect_equal(a1, sval3)
    expect_equal(a2, sval3)
    expect_equal(a3, sval3)
    expect_equal(a4, sval0)
    expect_equal(a5, sval0)
    
    expect_equal(b1, dval3)
    expect_equal(b2, dval3)
    expect_equal(b3, dval3)
    expect_equal(b4, dval0)
    expect_equal(b5, dval0)
})

## #########################################################

## Coercion - list (defining directed acyclic graph) to matrix

dval3 <-
    structure(c(0, 1, 1, 0, 0, 0, 0, 0, 0), .Dim = c(3L, 3L), .Dimnames = list(
    c("1", "2", "3"), c("1", "2", "3")))

dval0 <-
    structure(numeric(0), .Dim = c(0L, 0L))

sval3 <- as(dval3, "dgCMatrix")
sval0 <- as(dval0, "dgCMatrix")

test_that("dagList2matrix", {
    a1 <- dagList2dgCMatrix__(ss, vn)
    a2 <- dagList2dgCMatrix__(ss, NULL)
    a3 <- dagList2dgCMatrix__(ss, c(1,2,3))
    a4 <- dagList2dgCMatrix__(NULL,NULL)
    a5 <- dagList2dgCMatrix__(NULL,c(1,2,3))
    
    b1 <- dagList2matrix__(ss, vn)
    b2 <- dagList2matrix__(ss, NULL)
    b3 <- dagList2matrix__(ss, c(1,2,3))
    b4 <- dagList2matrix__(NULL,NULL)
    b5 <- dagList2matrix__(NULL,c(1,2,3))
    
    expect_equal(a1, sval3)
    expect_equal(a2, sval3)
    expect_equal(a3, sval3)
    expect_equal(a4, sval0)
    expect_equal(a5, sval0)
    
    expect_equal(b1, dval3)
    expect_equal(b2, dval3)
    expect_equal(b3, dval3)
    expect_equal(b4, dval0)
    expect_equal(b5, dval0)    
})
