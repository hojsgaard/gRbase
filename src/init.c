#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void C_isin(void *, void *, void *, void *, void *, void *);
extern void C_maxset(void *, void *, void *, void *);
extern void C_minset(void *, void *, void *, void *);
extern void combnC(void *, void *, void *, void *);

/* .Call calls */
extern SEXP R_colSums(SEXP);
extern SEXP R_colwiseProd(SEXP, SEXP);
extern SEXP R_rowSums(SEXP);
extern SEXP gRbase_M2dgCMatrix_(SEXP);
extern SEXP gRbase_M2matrix_(SEXP);
extern SEXP gRbase_MAT2ftM_(SEXP);
extern SEXP gRbase_RcppExport_registerCCallable();
extern SEXP gRbase_adjList2dgCMatrix(SEXP);
extern SEXP gRbase_adjList2ftList(SEXP);
extern SEXP gRbase_adjList2ftM(SEXP);
extern SEXP gRbase_adjList2matrix(SEXP);
extern SEXP gRbase_adjList2tfList(SEXP);
extern SEXP gRbase_adjList2tfM(SEXP);
extern SEXP gRbase_allSubsets0__(SEXP);
extern SEXP gRbase_allSubsets__(SEXP);
extern SEXP gRbase_cell2entry_(SEXP, SEXP);
extern SEXP gRbase_colmat2list(SEXP);
extern SEXP gRbase_dagList2dgCMatrix(SEXP, SEXP);
extern SEXP gRbase_dagList2matrix(SEXP, SEXP);
extern SEXP gRbase_dimnames_match_(SEXP, SEXP, SEXP);
extern SEXP gRbase_do_getcq_dense(SEXP, SEXP);
extern SEXP gRbase_do_getcq_sparse(SEXP, SEXP);
extern SEXP gRbase_do_mcs_dense(SEXP, SEXP);
extern SEXP gRbase_do_mcs_sparse(SEXP, SEXP);
extern SEXP gRbase_do_triangulate_elo(SEXP, SEXP);
extern SEXP gRbase_getCliquesDec_(SEXP, SEXP);
extern SEXP gRbase_get_cell_number_(SEXP, SEXP, SEXP);
extern SEXP gRbase_get_subset_(SEXP, SEXP, SEXP);
extern SEXP gRbase_get_superset_(SEXP, SEXP, SEXP);
extern SEXP gRbase_is_dimnames_(SEXP);
extern SEXP gRbase_is_named_array_(SEXP);
extern SEXP gRbase_is_number_vector_(SEXP);
extern SEXP gRbase_is_subsetof_(SEXP, SEXP);
extern SEXP gRbase_isadjMAT_(SEXP);
extern SEXP gRbase_isdagMAT_(SEXP);
extern SEXP gRbase_issymMAT_(SEXP);
extern SEXP gRbase_isugMAT_(SEXP);
extern SEXP gRbase_mcsMAT0_(SEXP, SEXP);
extern SEXP gRbase_mcsMAT_(SEXP, SEXP);
extern SEXP gRbase_moralizeMAT(SEXP);
extern SEXP gRbase_names2pairsM(SEXP, SEXP, SEXP, SEXP);
extern SEXP gRbase_next_cell_(SEXP, SEXP);
extern SEXP gRbase_next_cell_slice_(SEXP, SEXP, SEXP);
extern SEXP gRbase_perm_cell_entries_(SEXP, SEXP);
extern SEXP gRbase_rip_internal(SEXP, SEXP, SEXP);
extern SEXP gRbase_rowmat2list(SEXP);
extern SEXP gRbase_slice2entry_(SEXP, SEXP, SEXP);
extern SEXP gRbase_solveSPD(SEXP);
extern SEXP gRbase_sp_setXtf1(SEXP, SEXP);
extern SEXP gRbase_symMAT2ftM_(SEXP);
extern SEXP gRbase_tab_add_(SEXP, SEXP);
extern SEXP gRbase_tab_align_(SEXP, SEXP);
extern SEXP gRbase_tab_div0_(SEXP, SEXP);
extern SEXP gRbase_tab_div_(SEXP, SEXP);
extern SEXP gRbase_tab_equal_(SEXP, SEXP, SEXP);
extern SEXP gRbase_tab_expand_(SEXP, SEXP);
extern SEXP gRbase_tab_list_add_(SEXP);
extern SEXP gRbase_tab_list_mult_(SEXP);
extern SEXP gRbase_tab_marg_(SEXP, SEXP);
extern SEXP gRbase_tab_mult_(SEXP, SEXP);
extern SEXP gRbase_tab_op_(SEXP, SEXP, SEXP);
extern SEXP gRbase_tab_perm_(SEXP, SEXP);
extern SEXP gRbase_tab_subt_(SEXP, SEXP);
extern SEXP gRbase_topoSortMAT_(SEXP);
extern SEXP gRbase_triangulateMAT_(SEXP, SEXP);
extern SEXP gRbase_ugList2dgCMatrix(SEXP, SEXP);
extern SEXP gRbase_ugList2matrix(SEXP, SEXP);
extern SEXP gRbase_which_matrix_index(SEXP);

static const R_CMethodDef CEntries[] = {
    {"C_isin",   (DL_FUNC) &C_isin,   6},
    {"C_maxset", (DL_FUNC) &C_maxset, 4},
    {"C_minset", (DL_FUNC) &C_minset, 4},
    {"combnC",   (DL_FUNC) &combnC,   4},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"R_colSums",                           (DL_FUNC) &R_colSums,                           1},
    {"R_colwiseProd",                       (DL_FUNC) &R_colwiseProd,                       2},
    {"R_rowSums",                           (DL_FUNC) &R_rowSums,                           1},
    {"gRbase_M2dgCMatrix_",                 (DL_FUNC) &gRbase_M2dgCMatrix_,                 1},
    {"gRbase_M2matrix_",                    (DL_FUNC) &gRbase_M2matrix_,                    1},
    {"gRbase_MAT2ftM_",                     (DL_FUNC) &gRbase_MAT2ftM_,                     1},
    {"gRbase_RcppExport_registerCCallable", (DL_FUNC) &gRbase_RcppExport_registerCCallable, 0},
    {"gRbase_adjList2dgCMatrix",            (DL_FUNC) &gRbase_adjList2dgCMatrix,            1},
    {"gRbase_adjList2ftList",               (DL_FUNC) &gRbase_adjList2ftList,               1},
    {"gRbase_adjList2ftM",                  (DL_FUNC) &gRbase_adjList2ftM,                  1},
    {"gRbase_adjList2matrix",               (DL_FUNC) &gRbase_adjList2matrix,               1},
    {"gRbase_adjList2tfList",               (DL_FUNC) &gRbase_adjList2tfList,               1},
    {"gRbase_adjList2tfM",                  (DL_FUNC) &gRbase_adjList2tfM,                  1},
    {"gRbase_allSubsets0__",                (DL_FUNC) &gRbase_allSubsets0__,                1},
    {"gRbase_allSubsets__",                 (DL_FUNC) &gRbase_allSubsets__,                 1},
    {"gRbase_cell2entry_",                  (DL_FUNC) &gRbase_cell2entry_,                  2},
    {"gRbase_colmat2list",                  (DL_FUNC) &gRbase_colmat2list,                  1},
    {"gRbase_dagList2dgCMatrix",            (DL_FUNC) &gRbase_dagList2dgCMatrix,            2},
    {"gRbase_dagList2matrix",               (DL_FUNC) &gRbase_dagList2matrix,               2},
    {"gRbase_dimnames_match_",              (DL_FUNC) &gRbase_dimnames_match_,              3},
    {"gRbase_do_getcq_dense",               (DL_FUNC) &gRbase_do_getcq_dense,               2},
    {"gRbase_do_getcq_sparse",              (DL_FUNC) &gRbase_do_getcq_sparse,              2},
    {"gRbase_do_mcs_dense",                 (DL_FUNC) &gRbase_do_mcs_dense,                 2},
    {"gRbase_do_mcs_sparse",                (DL_FUNC) &gRbase_do_mcs_sparse,                2},
    {"gRbase_do_triangulate_elo",           (DL_FUNC) &gRbase_do_triangulate_elo,           2},
    {"gRbase_getCliquesDec_",               (DL_FUNC) &gRbase_getCliquesDec_,               2},
    {"gRbase_get_cell_number_",             (DL_FUNC) &gRbase_get_cell_number_,             3},
    {"gRbase_get_subset_",                  (DL_FUNC) &gRbase_get_subset_,                  3},
    {"gRbase_get_superset_",                (DL_FUNC) &gRbase_get_superset_,                3},
    {"gRbase_is_dimnames_",                 (DL_FUNC) &gRbase_is_dimnames_,                 1},
    {"gRbase_is_named_array_",              (DL_FUNC) &gRbase_is_named_array_,              1},
    {"gRbase_is_number_vector_",            (DL_FUNC) &gRbase_is_number_vector_,            1},
    {"gRbase_is_subsetof_",                 (DL_FUNC) &gRbase_is_subsetof_,                 2},
    {"gRbase_isadjMAT_",                    (DL_FUNC) &gRbase_isadjMAT_,                    1},
    {"gRbase_isdagMAT_",                    (DL_FUNC) &gRbase_isdagMAT_,                    1},
    {"gRbase_issymMAT_",                    (DL_FUNC) &gRbase_issymMAT_,                    1},
    {"gRbase_isugMAT_",                     (DL_FUNC) &gRbase_isugMAT_,                     1},
    {"gRbase_mcsMAT0_",                     (DL_FUNC) &gRbase_mcsMAT0_,                     2},
    {"gRbase_mcsMAT_",                      (DL_FUNC) &gRbase_mcsMAT_,                      2},
    {"gRbase_moralizeMAT",                  (DL_FUNC) &gRbase_moralizeMAT,                  1},
    {"gRbase_names2pairsM",                 (DL_FUNC) &gRbase_names2pairsM,                 4},
    {"gRbase_next_cell_",                   (DL_FUNC) &gRbase_next_cell_,                   2},
    {"gRbase_next_cell_slice_",             (DL_FUNC) &gRbase_next_cell_slice_,             3},
    {"gRbase_perm_cell_entries_",           (DL_FUNC) &gRbase_perm_cell_entries_,           2},
    {"gRbase_rip_internal",                 (DL_FUNC) &gRbase_rip_internal,                 3},
    {"gRbase_rowmat2list",                  (DL_FUNC) &gRbase_rowmat2list,                  1},
    {"gRbase_slice2entry_",                 (DL_FUNC) &gRbase_slice2entry_,                 3},
    {"gRbase_solveSPD",                     (DL_FUNC) &gRbase_solveSPD,                     1},
    {"gRbase_sp_setXtf1",                   (DL_FUNC) &gRbase_sp_setXtf1,                   2},
    {"gRbase_symMAT2ftM_",                  (DL_FUNC) &gRbase_symMAT2ftM_,                  1},
    {"gRbase_tab_add_",                     (DL_FUNC) &gRbase_tab_add_,                     2},
    {"gRbase_tab_align_",                   (DL_FUNC) &gRbase_tab_align_,                   2},
    {"gRbase_tab_div0_",                    (DL_FUNC) &gRbase_tab_div0_,                    2},
    {"gRbase_tab_div_",                     (DL_FUNC) &gRbase_tab_div_,                     2},
    {"gRbase_tab_equal_",                   (DL_FUNC) &gRbase_tab_equal_,                   3},
    {"gRbase_tab_expand_",                  (DL_FUNC) &gRbase_tab_expand_,                  2},
    {"gRbase_tab_list_add_",                (DL_FUNC) &gRbase_tab_list_add_,                1},
    {"gRbase_tab_list_mult_",               (DL_FUNC) &gRbase_tab_list_mult_,               1},
    {"gRbase_tab_marg_",                    (DL_FUNC) &gRbase_tab_marg_,                    2},
    {"gRbase_tab_mult_",                    (DL_FUNC) &gRbase_tab_mult_,                    2},
    {"gRbase_tab_op_",                      (DL_FUNC) &gRbase_tab_op_,                      3},
    {"gRbase_tab_perm_",                    (DL_FUNC) &gRbase_tab_perm_,                    2},
    {"gRbase_tab_subt_",                    (DL_FUNC) &gRbase_tab_subt_,                    2},
    {"gRbase_topoSortMAT_",                 (DL_FUNC) &gRbase_topoSortMAT_,                 1},
    {"gRbase_triangulateMAT_",              (DL_FUNC) &gRbase_triangulateMAT_,              2},
    {"gRbase_ugList2dgCMatrix",             (DL_FUNC) &gRbase_ugList2dgCMatrix,             2},
    {"gRbase_ugList2matrix",                (DL_FUNC) &gRbase_ugList2matrix,                2},
    {"gRbase_which_matrix_index",           (DL_FUNC) &gRbase_which_matrix_index,           1},
    {NULL, NULL, 0}
};

void R_init_gRbase(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
