\name{gRbase-internal}

%% ---------------------------------------
%% Coercion between graph representations.
%% ---------------------------------------
\alias{matrix2dgCMatrix}
\alias{dgCMatrix2matrix}

%\alias{MAT2dgCMatrix}
%\alias{MAT2matrix}
\alias{MAT2ftM_}
\alias{MAT2MAT}
\alias{as.amat}
\alias{symMAT2ftM_}

%% \alias{coerceGraph}
%% \alias{coerceGraph.graphNEL}
%% \alias{coerceGraph.igraph}
%% \alias{coerceGraph.matrix}
%% \alias{coerceGraph.dgCMatrix}

\alias{adjList2matrix}
\alias{adjList2dgCMatrix}

%%\alias{stepwise}

\alias{\%>\%}

%% Various R/C/C++ implementations;
%% --------------------------------
%% no need to document further

\alias{isdagMAT_}
\alias{issymMAT_}
\alias{isugMAT_}
\alias{aperm__}

\alias{as.named.array}

%\alias{cell2entry2_cpp}
%\alias{cell2entry_cpp}
%\alias{getCellNumberPrim_cpp}
%\alias{getCellNumber_cpp}
%\alias{nextCellSlicePrim_cpp}
%\alias{nextCellSlice_cpp}
%\alias{nextCell_cpp}
%\alias{permuteCellEntries_cpp}
%\alias{slice2entry_cpp}


\alias{alignArrays}
\alias{alignArrays2}
\alias{allSubsets}
\alias{allSubsets0}
\alias{arrayMargin}
\alias{arrayMargin0}
\alias{arrayMarginChr_cpp}
\alias{arrayOp}
\alias{arrayOp2}
\alias{arrayPerm}
\alias{arrayPerm0}
\alias{extendArrayAlong}
\alias{extendArrays}
\alias{getPerm}
\alias{fitLoglin}

\alias{propagateLS__}

\alias{isadjMAT_}
\alias{mcsMAT_}
\alias{mcsMAT0_}
\alias{topoSortMAT_}
\alias{triangulateMAT_}
\alias{triangulateEloMAT_}
\alias{do_triangulate_elo}
\alias{do_getcq_dense}
\alias{do_getcq_sparse}
\alias{do_mcs_dense}
\alias{do_mcs_sparse}
\alias{getCliquesDec_}
\alias{rip_internal}


\alias{sp_setXtf1} % bruges i gRain
\alias{solveSPD}   % bruges i gRim

%% Needs documentation
%%\alias{adjList2adjMAT}
\alias{adjList2ftList}
\alias{adjList2ftM}
\alias{adjList2tfList}
\alias{adjList2tfM}
\alias{glist2setMAT}

%% \alias{adjList2MAT} We need this

%% Some methods which need not be in the proper .Rd files (to shorten
%% the listing)


\alias{varNames.default}
\alias{nLevels.default}
\alias{valueLabels.default}

\alias{print.parray}

\alias{print.ripOrder}
\alias{plot.ripOrder}
\alias{maxClique}

%% utilities - deprecated; should go when downstreams have been updated
\alias{colSumsPrim}
\alias{intersectPrim}
\alias{matchPrim}
\alias{outerPrim}
\alias{setdiffPrim}
\alias{uniquePrim}
\alias{unlistPrim}
\alias{rowSumsPrim}



\alias{lapplyV2I}
\alias{lapplyI2V}
\alias{lapplyMatch}
\alias{isGSD_glist}
\alias{properties_glist}


%% ----------------------
%% Array operations in C
%% ----------------------
%% Examples of the usage of these functions are given in vignette
\alias{aperm2}
\alias{cell2entry}
\alias{entry2cell}
\alias{factGrid}
\alias{getCellNumberC}
\alias{slice2entry}
\alias{nextCell}
\alias{nextCellSlice}
\alias{permuteCellEntries}






%%% --------------
%%% Miscellaneous
%%% --------------

\alias{makeDimNames}
\alias{names2pairs}
\alias{names2pairsM}
\alias{rowmat2list}
\alias{colmat2list}
\alias{matrix2list}

\alias{list2rhsFormula}
\alias{rhsFormula2list}
\alias{list2rhsf}
\alias{rhsf2list}

\alias{which.arr.ind}
\alias{which_matrix_index}
\alias{pairs2num}


%% DED stuff
\alias{showf}  % used in old gRbase stuff; delete later
\alias{readf}  % used in old gRbase stuff; delete later
\alias{dual.rep}
%% !!

%% ggm related stuff
\alias{fit.ggm}
\alias{ggm}
\alias{ips}
\alias{outfun}
\alias{ell}
\alias{ellK}

%% !!

\alias{addEdge}
\alias{dropEdge}
\alias{addEdge.gModel}
\alias{dropEdge.gModel}
\alias{all.subsets}
\alias{extract.power}
\alias{selectOrder}
\alias{processFormula}
\alias{partial.corr.matrix}
\alias{update.gModel}
\alias{gRfit-class}

\alias{igraph-class}


\title{Internal gRbase functions}
\description{
  Internal functions called by other functions.
}
\keyword{internal}

%%\alias{showg}
%%\alias{readg}
%%\alias{varset}
%%\alias{in.list}
%%\alias{is.cont}
%%\alias{contains}
%%\alias{is.graphical}



%% Some igraph stuff. Should be removed one day
%% \alias{mcs.igraph}
%% \alias{mcsmarked.igraph}
%% \alias{junctionTree.igraph}
%% \alias{jTree.igraph}
%% \alias{rip.igraph}
%% \alias{moralize.igraph}
%% \alias{triangulate.igraph}

%% Needs .Rd - New Cpp implementations
%% -----------------------------------
%% Undocumented utilities
%% \alias{sp_getXi}
%% \alias{sp_getXj}
%% \alias{sp_getXij}
%% \alias{sp_getXtf}
%% \alias{sp_getXM}
%% \alias{sp_setXij1}
%% \alias{sp_setXtf1}
%% \alias{sp_setXM1}
%% \alias{sp_moralize}
%% \alias{sp_fromto}
%% \alias{st_fromto}

%% \alias{ripMAT2}
%% \alias{moralMAT}

%% \alias{topoSortMAT_sp}
%% \alias{topoSortMAT_st}

%% \alias{mcsMAT_spCpp}
%% \alias{mcsMAT_stCpp}

%% \alias{moralizeMAT_stR}
%% \alias{moralizeMAT_spC}
