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
extern SEXP _gRbase_M2dgCMatrix__(SEXP);
extern SEXP _gRbase_M2matrix__(SEXP);
extern SEXP _gRbase_MAT2ftM_(SEXP);
extern SEXP _gRbase_RcppExport_registerCCallable();
extern SEXP _gRbase_adjList2dgCMatrix__(SEXP);
extern SEXP _gRbase_adjList2ftList__(SEXP);
extern SEXP _gRbase_adjList2ftM__(SEXP);
extern SEXP _gRbase_adjList2matrix__(SEXP);
extern SEXP _gRbase_adjList2tfList__(SEXP);
extern SEXP _gRbase_adjList2tfM__(SEXP);
extern SEXP _gRbase_allSubsets0__(SEXP);
extern SEXP _gRbase_allSubsets__(SEXP);
extern SEXP _gRbase_all_pairs__(SEXP, SEXP, SEXP, SEXP);
extern SEXP _gRbase_cell2entry_(SEXP, SEXP);
extern SEXP _gRbase_cell2entry_prim_(SEXP, SEXP);
extern SEXP _gRbase_colmat2list__(SEXP);
extern SEXP _gRbase_dagList2dgCMatrix__(SEXP, SEXP);
extern SEXP _gRbase_dagList2matrix__(SEXP, SEXP);
extern SEXP _gRbase_dgCMatrix2matrix__(SEXP);
extern SEXP _gRbase_dimnames_match_(SEXP, SEXP, SEXP);
extern SEXP _gRbase_entry2cell_(SEXP, SEXP);
extern SEXP _gRbase_entry2cell_prim_(SEXP, SEXP);
extern SEXP _gRbase_getCliquesDec__(SEXP, SEXP);
extern SEXP _gRbase_get_cell_number_(SEXP, SEXP, SEXP);
extern SEXP _gRbase_get_subset_(SEXP, SEXP, SEXP);
extern SEXP _gRbase_get_subset__(SEXP, SEXP, SEXP);
extern SEXP _gRbase_get_superset_(SEXP, SEXP, SEXP);
extern SEXP _gRbase_get_superset__(SEXP, SEXP, SEXP);
extern SEXP _gRbase_is_dimnames_(SEXP);
extern SEXP _gRbase_is_named_array_(SEXP);
extern SEXP _gRbase_is_number_vector_(SEXP);
extern SEXP _gRbase_is_subsetof_(SEXP, SEXP);
extern SEXP _gRbase_is_subsetof__(SEXP, SEXP);
extern SEXP _gRbase_isadjMAT_(SEXP);
extern SEXP _gRbase_isdagMAT_(SEXP);
extern SEXP _gRbase_issymMAT_(SEXP);
extern SEXP _gRbase_isugMAT_(SEXP);
extern SEXP _gRbase_make_prod(SEXP, SEXP);
extern SEXP _gRbase_matrix2dgCMatrix__(SEXP);
extern SEXP _gRbase_mcsMAT__(SEXP, SEXP);
extern SEXP _gRbase_moralizeMAT(SEXP);
extern SEXP _gRbase_next_cell_(SEXP, SEXP);
extern SEXP _gRbase_next_cell_slice_(SEXP, SEXP, SEXP);
extern SEXP _gRbase_perm_cell_entries_(SEXP, SEXP);
extern SEXP _gRbase_rip_internal(SEXP, SEXP, SEXP);
extern SEXP _gRbase_rowmat2list__(SEXP);
extern SEXP _gRbase_slice2entry_(SEXP, SEXP, SEXP);
extern SEXP _gRbase_solveSPD(SEXP);
extern SEXP _gRbase_symMAT2ftM_(SEXP);
extern SEXP _gRbase_tabDiv0__(SEXP, SEXP);
extern SEXP _gRbase_tabMarg__(SEXP, SEXP);
extern SEXP _gRbase_tabMult__(SEXP, SEXP);
extern SEXP _gRbase_tab_add_(SEXP, SEXP);
extern SEXP _gRbase_tab_align_(SEXP, SEXP);
extern SEXP _gRbase_tab_div0_(SEXP, SEXP);
extern SEXP _gRbase_tab_div_(SEXP, SEXP);
extern SEXP _gRbase_tab_equal_(SEXP, SEXP, SEXP);
extern SEXP _gRbase_tab_expand_(SEXP, SEXP);
extern SEXP _gRbase_tab_list_add_(SEXP);
extern SEXP _gRbase_tab_list_mult_(SEXP);
extern SEXP _gRbase_tab_marg_(SEXP, SEXP);
extern SEXP _gRbase_tab_mult_(SEXP, SEXP);
extern SEXP _gRbase_tab_op_(SEXP, SEXP, SEXP);
extern SEXP _gRbase_tab_perm_(SEXP, SEXP);
extern SEXP _gRbase_tab_subt_(SEXP, SEXP);
extern SEXP _gRbase_topoSortMAT_(SEXP);
extern SEXP _gRbase_triang_elo_MAT__(SEXP, SEXP);
extern SEXP _gRbase_triang_mcwh_MAT__(SEXP, SEXP);
extern SEXP _gRbase_triangulateMAT__(SEXP, SEXP);
extern SEXP _gRbase_ugList2dgCMatrix__(SEXP, SEXP);
extern SEXP _gRbase_ugList2matrix__(SEXP, SEXP);
extern SEXP _gRbase_which_matrix_index__(SEXP);

static const R_CMethodDef CEntries[] = {
    {"C_isin",   (DL_FUNC) &C_isin,   6},
    {"C_maxset", (DL_FUNC) &C_maxset, 4},
    {"C_minset", (DL_FUNC) &C_minset, 4},
    {"combnC",   (DL_FUNC) &combnC,   4},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"R_colSums",                            (DL_FUNC) &R_colSums,                            1},
    {"R_colwiseProd",                        (DL_FUNC) &R_colwiseProd,                        2},
    {"R_rowSums",                            (DL_FUNC) &R_rowSums,                            1},
    {"_gRbase_M2dgCMatrix__",                (DL_FUNC) &_gRbase_M2dgCMatrix__,                1},
    {"_gRbase_M2matrix__",                   (DL_FUNC) &_gRbase_M2matrix__,                   1},
    {"_gRbase_MAT2ftM_",                     (DL_FUNC) &_gRbase_MAT2ftM_,                     1},
    {"_gRbase_RcppExport_registerCCallable", (DL_FUNC) &_gRbase_RcppExport_registerCCallable, 0},
    {"_gRbase_adjList2dgCMatrix__",          (DL_FUNC) &_gRbase_adjList2dgCMatrix__,          1},
    {"_gRbase_adjList2ftList__",             (DL_FUNC) &_gRbase_adjList2ftList__,             1},
    {"_gRbase_adjList2ftM__",                (DL_FUNC) &_gRbase_adjList2ftM__,                1},
    {"_gRbase_adjList2matrix__",             (DL_FUNC) &_gRbase_adjList2matrix__,             1},
    {"_gRbase_adjList2tfList__",             (DL_FUNC) &_gRbase_adjList2tfList__,             1},
    {"_gRbase_adjList2tfM__",                (DL_FUNC) &_gRbase_adjList2tfM__,                1},
    {"_gRbase_allSubsets0__",                (DL_FUNC) &_gRbase_allSubsets0__,                1},
    {"_gRbase_allSubsets__",                 (DL_FUNC) &_gRbase_allSubsets__,                 1},
    {"_gRbase_all_pairs__",                  (DL_FUNC) &_gRbase_all_pairs__,                  4},
    {"_gRbase_cell2entry_",                  (DL_FUNC) &_gRbase_cell2entry_,                  2},
    {"_gRbase_cell2entry_prim_",             (DL_FUNC) &_gRbase_cell2entry_prim_,             2},
    {"_gRbase_colmat2list__",                (DL_FUNC) &_gRbase_colmat2list__,                1},
    {"_gRbase_dagList2dgCMatrix__",          (DL_FUNC) &_gRbase_dagList2dgCMatrix__,          2},
    {"_gRbase_dagList2matrix__",             (DL_FUNC) &_gRbase_dagList2matrix__,             2},
    {"_gRbase_dgCMatrix2matrix__",           (DL_FUNC) &_gRbase_dgCMatrix2matrix__,           1},
    {"_gRbase_dimnames_match_",              (DL_FUNC) &_gRbase_dimnames_match_,              3},
    {"_gRbase_entry2cell_",                  (DL_FUNC) &_gRbase_entry2cell_,                  2},
    {"_gRbase_entry2cell_prim_",             (DL_FUNC) &_gRbase_entry2cell_prim_,             2},
    {"_gRbase_getCliquesDec__",              (DL_FUNC) &_gRbase_getCliquesDec__,              2},
    {"_gRbase_get_cell_number_",             (DL_FUNC) &_gRbase_get_cell_number_,             3},
    {"_gRbase_get_subset_",                  (DL_FUNC) &_gRbase_get_subset_,                  3},
    {"_gRbase_get_subset__",                 (DL_FUNC) &_gRbase_get_subset__,                 3},
    {"_gRbase_get_superset_",                (DL_FUNC) &_gRbase_get_superset_,                3},
    {"_gRbase_get_superset__",               (DL_FUNC) &_gRbase_get_superset__,               3},
    {"_gRbase_is_dimnames_",                 (DL_FUNC) &_gRbase_is_dimnames_,                 1},
    {"_gRbase_is_named_array_",              (DL_FUNC) &_gRbase_is_named_array_,              1},
    {"_gRbase_is_number_vector_",            (DL_FUNC) &_gRbase_is_number_vector_,            1},
    {"_gRbase_is_subsetof_",                 (DL_FUNC) &_gRbase_is_subsetof_,                 2},
    {"_gRbase_is_subsetof__",                (DL_FUNC) &_gRbase_is_subsetof__,                2},
    {"_gRbase_isadjMAT_",                    (DL_FUNC) &_gRbase_isadjMAT_,                    1},
    {"_gRbase_isdagMAT_",                    (DL_FUNC) &_gRbase_isdagMAT_,                    1},
    {"_gRbase_issymMAT_",                    (DL_FUNC) &_gRbase_issymMAT_,                    1},
    {"_gRbase_isugMAT_",                     (DL_FUNC) &_gRbase_isugMAT_,                     1},
    {"_gRbase_make_prod",                    (DL_FUNC) &_gRbase_make_prod,                    2},
    {"_gRbase_matrix2dgCMatrix__",           (DL_FUNC) &_gRbase_matrix2dgCMatrix__,           1},
    {"_gRbase_mcsMAT__",                     (DL_FUNC) &_gRbase_mcsMAT__,                     2},
    {"_gRbase_moralizeMAT",                  (DL_FUNC) &_gRbase_moralizeMAT,                  1},
    {"_gRbase_next_cell_",                   (DL_FUNC) &_gRbase_next_cell_,                   2},
    {"_gRbase_next_cell_slice_",             (DL_FUNC) &_gRbase_next_cell_slice_,             3},
    {"_gRbase_perm_cell_entries_",           (DL_FUNC) &_gRbase_perm_cell_entries_,           2},
    {"_gRbase_rip_internal",                 (DL_FUNC) &_gRbase_rip_internal,                 3},
    {"_gRbase_rowmat2list__",                (DL_FUNC) &_gRbase_rowmat2list__,                1},
    {"_gRbase_slice2entry_",                 (DL_FUNC) &_gRbase_slice2entry_,                 3},
    {"_gRbase_solveSPD",                     (DL_FUNC) &_gRbase_solveSPD,                     1},
    {"_gRbase_symMAT2ftM_",                  (DL_FUNC) &_gRbase_symMAT2ftM_,                  1},
    {"_gRbase_tabDiv0__",                    (DL_FUNC) &_gRbase_tabDiv0__,                    2},
    {"_gRbase_tabMarg__",                    (DL_FUNC) &_gRbase_tabMarg__,                    2},
    {"_gRbase_tabMult__",                    (DL_FUNC) &_gRbase_tabMult__,                    2},
    {"_gRbase_tab_add_",                     (DL_FUNC) &_gRbase_tab_add_,                     2},
    {"_gRbase_tab_align_",                   (DL_FUNC) &_gRbase_tab_align_,                   2},
    {"_gRbase_tab_div0_",                    (DL_FUNC) &_gRbase_tab_div0_,                    2},
    {"_gRbase_tab_div_",                     (DL_FUNC) &_gRbase_tab_div_,                     2},
    {"_gRbase_tab_equal_",                   (DL_FUNC) &_gRbase_tab_equal_,                   3},
    {"_gRbase_tab_expand_",                  (DL_FUNC) &_gRbase_tab_expand_,                  2},
    {"_gRbase_tab_list_add_",                (DL_FUNC) &_gRbase_tab_list_add_,                1},
    {"_gRbase_tab_list_mult_",               (DL_FUNC) &_gRbase_tab_list_mult_,               1},
    {"_gRbase_tab_marg_",                    (DL_FUNC) &_gRbase_tab_marg_,                    2},
    {"_gRbase_tab_mult_",                    (DL_FUNC) &_gRbase_tab_mult_,                    2},
    {"_gRbase_tab_op_",                      (DL_FUNC) &_gRbase_tab_op_,                      3},
    {"_gRbase_tab_perm_",                    (DL_FUNC) &_gRbase_tab_perm_,                    2},
    {"_gRbase_tab_subt_",                    (DL_FUNC) &_gRbase_tab_subt_,                    2},
    {"_gRbase_topoSortMAT_",                 (DL_FUNC) &_gRbase_topoSortMAT_,                 1},
    {"_gRbase_triang_elo_MAT__",             (DL_FUNC) &_gRbase_triang_elo_MAT__,             2},
    {"_gRbase_triang_mcwh_MAT__",            (DL_FUNC) &_gRbase_triang_mcwh_MAT__,            2},
    {"_gRbase_triangulateMAT__",             (DL_FUNC) &_gRbase_triangulateMAT__,             2},
    {"_gRbase_ugList2dgCMatrix__",           (DL_FUNC) &_gRbase_ugList2dgCMatrix__,           2},
    {"_gRbase_ugList2matrix__",              (DL_FUNC) &_gRbase_ugList2matrix__,              2},
    {"_gRbase_which_matrix_index__",         (DL_FUNC) &_gRbase_which_matrix_index__,         1},
    {NULL, NULL, 0}
};

void R_init_gRbase(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
