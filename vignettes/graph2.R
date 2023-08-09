### R code from vignette source 'graph2.Rnw'

###################################################
### code chunk number 1: graph2_src.rnw:2-5
###################################################
options("width"=80)
library(igraph)
ps.options(family="serif")


###################################################
### code chunk number 2: graph2_src.rnw:118-124
###################################################
library(gRbase)
ug0 <- gRbase::ug(~a:b, ~b:c:d, ~e)
ug0 <- gRbase::ug(~a:b + b:c:d + e)
ug0 <- gRbase::ug(~a*b + b*c*d + e)
ug0 <- gRbase::ug(c("a", "b"), c("b", "c", "d"), "e")
ug0


###################################################
### code chunk number 3: graph2_src.rnw:127-128
###################################################
plot(ug0)


###################################################
### code chunk number 4: graph2_src.rnw:142-148
###################################################
myplot <- function(x, layout=layout.fruchterman.reingold(x), ...) {
  V(x)$size <- 30
  V(x)$label.cex <- 3
  plot(x, layout=layout, ...)
  return(invisible())
}


###################################################
### code chunk number 5: graph2_src.rnw:154-155
###################################################
myplot(ug0)


###################################################
### code chunk number 6: graph2_src.rnw:164-166
###################################################
ug0i <- gRbase::ug(~a:b + b:c:d + e, result="matrix")
ug0i


###################################################
### code chunk number 7: graph2_src.rnw:171-174
###################################################
as(ug0, "matrix")
as(ug0, "dgCMatrix")
as(ug0i, "igraph")


###################################################
### code chunk number 8: graph2_src.rnw:186-189
###################################################
## Using gRbase
ug0a <- gRbase::addEdge("a", "c", ug0)
ug0a <- gRbase::removeEdge("c", "d", ug0)


###################################################
### code chunk number 9: graph2_src.rnw:193-196
###################################################
## Using igraph
ug0a <- igraph::add_edges(ug0, c("a", "c"))
ug0a <- igraph::delete_edges(ug0, c("c|d"))


###################################################
### code chunk number 10: graph2_src.rnw:204-207
###################################################
## Using gRbase
gRbase::nodes(ug0)           
gRbase::edges(ug0) |> str()


###################################################
### code chunk number 11: graph2_src.rnw:210-215
###################################################
## Using igraph
igraph::V(ug0)
igraph::V(ug0) |> attr("names")
igraph::E(ug0)
igraph::E(ug0) |> attr("vnames")


###################################################
### code chunk number 12: graph2_src.rnw:219-224
###################################################
gRbase::maxClique(ug0) ## |> str() 
gRbase::get_cliques(ug0) |> str()
## Using igraph
igraph::max_cliques(ug0) |>
    lapply(function(x) attr(x, "names"))  |> str()


###################################################
### code chunk number 13: graph2_src.rnw:240-241
###################################################
gRbase::separates("a", "d", c("b", "c"), ug0)


###################################################
### code chunk number 14: graph2_src.rnw:254-255
###################################################
ug1 <- gRbase::subGraph(c("b", "c", "d", "e"), ug0)


###################################################
### code chunk number 15: graph2_src.rnw:258-259
###################################################
ug12 <- igraph::subgraph(ug0, c("b", "c", "d", "e"))


###################################################
### code chunk number 16: graph2_src.rnw:262-264
###################################################
par(mfrow=c(1,2), mar=c(0,0,0,0))
myplot(ug1); myplot(ug12)


###################################################
### code chunk number 17: graph2_src.rnw:276-278
###################################################
gRbase::adj(ug0, "c")
gRbase::closure("c", ug0)


###################################################
### code chunk number 18: graph2_src.rnw:296-302
###################################################
dag0 <- gRbase::dag(~a, ~b*a,  ~c*a*b, ~d*c*e, ~e*a, ~g*f)
dag0 <- gRbase::dag(~a + b*a + c*a*b + d*c*e + e*a + g*f)
dag0 <- gRbase::dag(~a + b|a + c|a*b + d|c*e + e|a + g|f)
dag0 <- gRbase::dag("a", c("b", "a"), c("c", "a", "b"), c("d", "c", "e"), 
            c("e", "a"), c("g", "f"))
dag0


###################################################
### code chunk number 19: graph2_src.rnw:316-317
###################################################
myplot(dag0)


###################################################
### code chunk number 20: graph2_src.rnw:324-326
###################################################
gRbase::nodes(dag0)
gRbase::edges(dag0) |> str()


###################################################
### code chunk number 21: graph2_src.rnw:333-334
###################################################
edgeList(dag0) |> str()


###################################################
### code chunk number 22: graph2_src.rnw:339-342
###################################################
vpardag0 <- gRbase::vpar(dag0)
vpardag0 |> str()
vpardag0$c


###################################################
### code chunk number 23: graph2_src.rnw:359-364
###################################################
gRbase::parents("d", dag0)
gRbase::children("c", dag0)
gRbase::ancestralSet(c("b", "e"), dag0)
ag <- gRbase::ancestralGraph(c("b", "e"), dag0)
myplot(ag)


###################################################
### code chunk number 24: graph2_src.rnw:376-378
###################################################
dag0m <- gRbase::moralize(dag0)
myplot(dag0m)


###################################################
### code chunk number 25: graph2_src.rnw:404-410
###################################################
adjm <- matrix(c(0, 1, 1, 1,
                 1, 0, 0, 1,
                 1, 0, 0, 1,
                 0, 1, 0, 0), byrow=TRUE, nrow=4)
rownames(adjm) <- colnames(adjm) <- letters[1:4]
adjm


###################################################
### code chunk number 26: graph2_src.rnw:419-422
###################################################
gG1 <- gG2 <- as(adjm, "igraph")
lay <- layout.fruchterman.reingold(gG1)
E(gG2)$arrow.mode <- c(2,0)[1+is.mutual(gG2)]


###################################################
### code chunk number 27: graph2_src.rnw:425-427
###################################################
par(mfrow=c(1,2), mar=c(0,0,0,0))
myplot(gG1, layout=lay); myplot(gG2, layout=lay)


###################################################
### code chunk number 28: graph2_src.rnw:440-450
###################################################
d1 <- matrix(0, 11, 11)
d1[1,2] <- d1[2,1] <- d1[1,3] <- d1[3,1] <- d1[2,4] <- d1[4,2] <- 
  d1[5,6] <- d1[6,5] <- 1
d1[9,10] <- d1[10,9] <- d1[7,8] <- d1[8,7] <- d1[3,5] <- 
  d1[5,10] <- d1[4,6] <- d1[4,7] <- 1
d1[6,11] <- d1[7,11] <- 1
rownames(d1) <- colnames(d1) <- letters[1:11]
cG1 <- as(d1, "igraph")
E(cG1)$arrow.mode <- c(2,0)[1+is.mutual(cG1)]
myplot(cG1, layout=layout.fruchterman.reingold)


###################################################
### code chunk number 29: graph2_src.rnw:561-562
###################################################
myplot(ug0)


###################################################
### code chunk number 30: graph2_src.rnw:566-567
###################################################
gRbase::separates("a", "d", "b", ug0) 


###################################################
### code chunk number 31: graph2_src.rnw:574-575
###################################################
gRbase::separates("a", "d", character(0), ug0)


###################################################
### code chunk number 32: graph2_src.rnw:610-616
###################################################
d_separates <- function(a, b, c, dag_) {
    ##ag <- ancestralGraph(union(union(a, b), c), dag_)
    ag <- ancestralGraph(c(a, b, c), dag_)
    separates(a, b, c, moralize(ag))
}
d_separates("c", "e", "a", dag0)    


###################################################
### code chunk number 33: graph2_src.rnw:685-687
###################################################
gRbase::is.simplicial("b", ug0)
gRbase::simplicialNodes(ug0)


###################################################
### code chunk number 34: graph2_src.rnw:694-697
###################################################
gRbase::connComp(ug0) |> str()
## Using igraph
igraph::components(ug0) |> str()


###################################################
### code chunk number 35: graph2_src.rnw:709-710
###################################################
gRbase::is.triangulated(ug0)


###################################################
### code chunk number 36: graph2_src.rnw:713-714
###################################################
igraph::is_chordal(ug0)


###################################################
### code chunk number 37: graph2_src.rnw:726-727
###################################################
gRbase::is.decomposition("a", "d", c("b", "c"), ug0) 


###################################################
### code chunk number 38: graph2_src.rnw:747-748
###################################################
myplot(ug0)


###################################################
### code chunk number 39: graph2_src.rnw:751-752
###################################################
gRbase::mcs(ug0)


###################################################
### code chunk number 40: graph2_src.rnw:755-757
###################################################
igraph::max_cardinality(ug0)
igraph::max_cardinality(ug0)$alpham1 |> attr("names")


###################################################
### code chunk number 41: graph2_src.rnw:764-765
###################################################
gRbase::mcs(ug0, root=c("d", "c", "a"))


###################################################
### code chunk number 42: graph2_src.rnw:788-789
###################################################
gRbase::rip(ug0)


###################################################
### code chunk number 43: graph2_src.rnw:795-801
###################################################
ug2 <- gRbase::ug(~a:b:c + c:d + d:e + a:e)
ug2 <- gRbase::ug(~a:b:c + c:d + d:e + e:f + a:f)

gRbase::is.triangulated(ug2)
igraph::is_chordal(ug2)  |> str()
myplot(ug2)


###################################################
### code chunk number 44: graph2_src.rnw:806-808
###################################################
ug3 <- gRbase::triangulate(ug2)
gRbase::is.triangulated(ug3)


###################################################
### code chunk number 45: graph2_src.rnw:812-815
###################################################
zzz <- igraph::is_chordal(ug2, fillin=TRUE, newgraph=TRUE)
V(ug2)[zzz$fillin]
ug32 <- zzz$newgraph


###################################################
### code chunk number 46: graph2_src.rnw:818-823
###################################################
par(mfrow=c(1,3), mar=c(0,0,0,0))
lay <- layout.fruchterman.reingold(ug2) 
myplot(ug2, layout=lay);
myplot(ug3, layout=lay);
myplot(ug32, layout=lay)


###################################################
### code chunk number 47: graph2_src.rnw:913-914 (eval = FALSE)
###################################################
## adj(moralize(dag0), "e")


###################################################
### code chunk number 48: graph2_src.rnw:1013-1016
###################################################
ug4 <- graph.formula(a -- b:c, c--b:d, e -- a:d) 
ug4
myplot(ug4)


###################################################
### code chunk number 49: graph2_src.rnw:1021-1025
###################################################
ug4.2 <- graph.empty(n=5, directed=FALSE)
V(ug4.2)$name <- V(ug4.2)$label <- letters[1:5]
ug4.2 <- add.edges(ug4.2, 1+c(0,1, 0,2, 0,4, 1,2, 2,3, 3,4))
ug4.2


###################################################
### code chunk number 50: graph2_src.rnw:1041-1042 (eval = FALSE)
###################################################
## myplot(ug4, layout=layout.graphopt)


###################################################
### code chunk number 51: graph2_src.rnw:1054-1061
###################################################
ug4$layout   <- layout.graphopt(ug4)
V(ug4)$label <- V(ug4)$name
V(ug4)$color <- "red"
V(ug4)[1]$color <- "green"
V(ug4)$size <- 40
V(ug4)$label.cex <- 3
plot(ug4)


###################################################
### code chunk number 52: graph2_src.rnw:1073-1077
###################################################
ug5 <- set.vertex.attribute(ug4, "discrete", value=c(T, T, F, F, T))
V(ug5)[discrete]$color <- "green"
V(ug5)[!discrete]$color <- "red"
plot(ug5)


###################################################
### code chunk number 53: graph2_src.rnw:1101-1103 (eval = FALSE)
###################################################
## xy <- tkplot.getcoords(2)
## plot(g, layout=xy)


###################################################
### code chunk number 54: graph2_src.rnw:1109-1110
###################################################
layout.fruchterman.reingold(ug4)


###################################################
### code chunk number 55: graph2_src.rnw:1117-1118
###################################################
ug4$layout <- layout.fruchterman.reingold


###################################################
### code chunk number 56: graph2_src.rnw:1123-1124
###################################################
ug4$layout <- layout.fruchterman.reingold(ug4)


###################################################
### code chunk number 57: samelay
###################################################
ug5 <- gRbase::ug(~A*B*C + B*C*D + D*E)
ug6 <- gRbase::ug(~A*B + B*C + C*D + D*E) 
lay.fr <- layout.fruchterman.reingold(ug5)
ug6$layout       <- ug5$layout       <- lay.fr
V(ug5)$size      <- V(ug6)$size      <- 50
V(ug5)$label.cex <- V(ug6)$label.cex <- 3
par(mfrow=c(1,2), mar=c(0,0,0,0))
plot(ug5); plot(ug6)


###################################################
### code chunk number 58: graph2_src.rnw:1160-1173
###################################################
em1 <- matrix(c(0, 1, 1, 0,
                0, 0, 0, 1,
                1, 0, 0, 1,
                0, 1, 0, 0), nrow=4, byrow=TRUE)
iG  <- graph.adjacency(em1) 
V(iG)$shape <- c("circle", "square", "circle", "square")  
V(iG)$color <- rep(c("red", "green"), 2)
V(iG)$label <- c("A", "B", "C", "D")
E(iG)$arrow.mode <- c(2,0)[1 + is.mutual(iG)]
E(iG)$color  <- rep(c("blue", "black"), 3)
E(iG)$curved <- c(T, F, F, F, F, F) 
iG$layout    <- layout.graphopt(iG)
myplot(iG)


###################################################
### code chunk number 59: graph2_src.rnw:1264-1265
###################################################
args(querygraph)


###################################################
### code chunk number 60: graph2_src.rnw:1270-1274
###################################################
ug_ <- gRbase::ug(~a:b + b:c:d + e)
gRbase::separates("a", "d", c("b", "c"), ug_)                   
gRbase::querygraph(ug_, "separates", "a", "d", c("b", "c"))
gRbase::qgraph(ug_, "separates", "a", "d", c("b", "c")) 


