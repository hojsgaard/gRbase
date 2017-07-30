\name{gRbase-internal}

%% FIXME: Old names to get gRain to work
\alias{get_subset_}
\alias{get_superset_}
\alias{is_subset_of_}



\alias{igraph-class}
\alias{\%>\%}

%% ---------------------------------------
%% Coercion between graph representations.
%% ---------------------------------------
\alias{matrix2dgCMatrix}
\alias{dgCMatrix2matrix}
\alias{adjList2ftM}
\alias{adjList2tfM}
\alias{MAT2ftM_}
\alias{symMAT2ftM_}

%% Various R/C/C++ implementations;
%% --------------------------------
%% no need to document further

\alias{isdagMAT_}
\alias{issymMAT_}
\alias{isugMAT_}
\alias{isadjMAT_}

\alias{mcsMAT__}

\alias{topoSortMAT_}
\alias{getCliquesDec__}
\alias{rip_internal}

\alias{triangulateMAT__}
\alias{triang_mcwh_MAT__}
\alias{triang_elo_MAT__}

\alias{sp_setXtf1} % bruges i gRain
\alias{solveSPD}   % bruges i gRim

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

\alias{intersectPrim}
\alias{matchPrim}
\alias{outerPrim}
\alias{setdiffPrim}
\alias{uniquePrim}
\alias{unlistPrim}

\alias{lapplyV2I}
\alias{lapplyI2V}
\alias{lapplyMatch}
\alias{isGSD_glist}
\alias{properties_glist}

%% ----------------------
%% Array operations in C
%% ----------------------
%% Examples of the usage of these functions are given in vignette

\alias{cell2entry}
\alias{entry2cell}
\alias{factGrid}
\alias{slice2entry}
\alias{nextCell}
\alias{nextCellSlice}


%%% --------------
%%% Miscellaneous
%%% --------------

\alias{makeDimNames}
\alias{list2rhsFormula}
\alias{rhsFormula2list}
\alias{list2rhsf}
\alias{rhsf2list}

\alias{pairs2num}



%% ggm related stuff
\alias{fit.ggm}
\alias{ggm}
\alias{ips}
\alias{outfun}
\alias{ell}
\alias{ellK}

%% DED stuff - used in old gRbase stuff; delete later
\alias{showf}  
\alias{readf}  
\alias{dual.rep}
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


\title{Internal gRbase functions}
\description{
  Internal functions called by other functions.
}
\keyword{internal}

