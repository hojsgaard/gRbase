context("gRbase misc")

##############################

UG <- ug(~a:b + b:c)

test_that("mcs()", {
    expect_equal(mcs(UG), c("a", "b", "c"))
})



dagNEL  <- dag(~ a:b:c + c:d:e, result="graphNEL")
ugNEL  <- ug(~a:b:c + c:d:e, result="graphNEL")
ug2NEL  <- ug(~ a:b + b:c + c:d + d:a, result="graphNEL") # four cycle

test_that("topo_sort()", {
    expect_equal(topo_sort(dagNEL), topoSort(dagNEL))
})


test_that("is_dag()", {
    ## Is graph a DAG?
    expect_true(is_dag(dagNEL))
    expect_true(is_dag(as(dagNEL, "igraph")))
    expect_true(is_dag(as(dagNEL, "matrix")))
    expect_true(is_dag(as(dagNEL, "Matrix")))
    
    expect_false(is_dag(ugNEL))
    expect_false(is_dag(as(ugNEL, "igraph")))
    expect_false(is_dag(as(ugNEL, "matrix")))
    expect_false(is_dag(as(ugNEL, "Matrix")))

    ## Is graph undirected?
    expect_false(is_dag(ugNEL))
    expect_false(is_dag(as(ugNEL, "igraph")))
    expect_false(is_dag(as(ugNEL, "matrix")))
    expect_false(is_dag(as(ugNEL, "Matrix")))
    
    expect_true(is_ug(ugNEL))
    expect_true(is_ug(as(ugNEL, "igraph")))
    expect_true(is_ug(as(ugNEL, "matrix")))
    expect_true(is_ug(as(ugNEL, "Matrix")))

    ## Is graph a triangulated (i.e. chordal) undirected graph
    expect_false(is_tug(dagNEL))
    expect_true(is_tug(ugNEL))

    ## Example where the graph is not triangulated
    expect_false(is_tug(ug2NEL))
    
    ## Bidirected graphs
    graph::edgemode(ugNEL)
    graph::edgemode(ugNEL) <- "directed"
    graph::edgemode(ugNEL)
    expect_false(is_dag(ugNEL))
    expect_true(is_ug(ugNEL))    
})



 



