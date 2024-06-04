#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _gRbase_adjList2dgCMatrix__(void *);
extern SEXP _gRbase_adjList2ftList__(void *);
extern SEXP _gRbase_adjList2ftM__(void *);
extern SEXP _gRbase_adjList2matrix__(void *);
extern SEXP _gRbase_adjList2tfList__(void *);
extern SEXP _gRbase_adjList2tfM__(void *);
extern SEXP _gRbase_all_pairs__(void *, void *, void *, void *);
extern SEXP _gRbase_allSubsets_(void *);
extern SEXP _gRbase_allSubsets0_(void *);
extern SEXP _gRbase_cell2entry_(void *, void *);
extern SEXP _gRbase_cell2entry_perm_(void *, void *, void *);
extern SEXP _gRbase_choose_(void *, void *);
extern SEXP _gRbase_colmat2list__(void *);
extern SEXP _gRbase_dagList2dgCMatrix__(void *, void *);
extern SEXP _gRbase_dagList2matrix__(void *, void *);
extern SEXP _gRbase_dimnames_match_(void *, void *, void *);
extern SEXP _gRbase_do_combn(void *, void *);
extern SEXP _gRbase_entry2cell_(void *, void *);
extern SEXP _gRbase_filter_maximal_vectors_(void *, void *);
extern SEXP _gRbase_get_subset_(void *, void *, void *);
extern SEXP _gRbase_get_superset_(void *, void *, void *);
extern SEXP _gRbase_getCliquesDec__(void *, void *);
extern SEXP _gRbase_is_dimnames_(void *);
extern SEXP _gRbase_is_named_array_(void *);
extern SEXP _gRbase_is_number_vector_(void *);
extern SEXP _gRbase_is_subsetof_(void *, void *);
extern SEXP _gRbase_isadjMAT_(void *);
extern SEXP _gRbase_isdagMAT_(void *);
extern SEXP _gRbase_isin_(void *, void *, void *);
extern SEXP _gRbase_issymMAT_(void *);
extern SEXP _gRbase_isugMAT_(void *);
extern SEXP _gRbase_M2dgCMatrix__(void *);
extern SEXP _gRbase_M2matrix__(void *);
extern SEXP _gRbase_make_plevels_(void *);
extern SEXP _gRbase_MAT2ftM_(void *);
extern SEXP _gRbase_max_set_(void *, void *);
extern SEXP _gRbase_mcsMAT__(void *, void *);
extern SEXP _gRbase_min_set_(void *, void *);
extern SEXP _gRbase_moralizeMAT(void *);
extern SEXP _gRbase_next_cell_(void *, void *);
extern SEXP _gRbase_next_cell_slice_(void *, void *, void *);
extern SEXP _gRbase_next_cell2_(void *, void *);
extern SEXP _gRbase_next_perm_(void *);
extern SEXP _gRbase_order2_(void *, void *);
extern SEXP _gRbase_perm_cell_entries_(void *, void *);
extern SEXP _gRbase_RcppExport_registerCCallable(void);
extern SEXP _gRbase_rip_internal(void *, void *, void *);
extern SEXP _gRbase_rowmat2list__(void *);
extern SEXP _gRbase_slice2entry_(void *, void *, void *);
extern SEXP _gRbase_solveSPD(void *);
extern SEXP _gRbase_symMAT2ftM_(void *);
extern SEXP _gRbase_tab_add_(void *, void *);
extern SEXP _gRbase_tab_align_(void *, void *);
extern SEXP _gRbase_tab_div_(void *, void *);
extern SEXP _gRbase_tab_div0_(void *, void *);
extern SEXP _gRbase_tab_equal_(void *, void *, void *);
extern SEXP _gRbase_tab_expand_(void *, void *, void *);
extern SEXP _gRbase_tab_list_add_(void *);
extern SEXP _gRbase_tab_list_mult_(void *);
extern SEXP _gRbase_tab_marg_(void *, void *);
extern SEXP _gRbase_tab_marg2_(void *, void *);
extern SEXP _gRbase_tab_mult_(void *, void *);
extern SEXP _gRbase_tab_op_(void *, void *, void *);
extern SEXP _gRbase_tab_perm_(void *, void *);
extern SEXP _gRbase_tab_subt_(void *, void *);
extern SEXP _gRbase_tabDiv0__(void *, void *);
extern SEXP _gRbase_tabMarg__(void *, void *);
extern SEXP _gRbase_tabMult__(void *, void *);
extern SEXP _gRbase_topo_sortMAT_(void *);
extern SEXP _gRbase_triang_elo_MAT__(void *, void *);
extern SEXP _gRbase_triang_mcwh_MAT__(void *, void *);
extern SEXP _gRbase_ugList2dgCMatrix__(void *, void *);
extern SEXP _gRbase_ugList2matrix__(void *, void *);
extern SEXP _gRbase_unlist_chr__(void *);
extern SEXP _gRbase_which_(void *);
extern SEXP _gRbase_which_matrix_index__(void *);
extern SEXP _gRbase_which2_(void *);
extern SEXP R_colSums(void *);
extern SEXP R_colwiseProd(void *, void *);
extern SEXP R_rowSums(void *);

static const R_CallMethodDef CallEntries[] = {
    {"_gRbase_adjList2dgCMatrix__",          (DL_FUNC) &_gRbase_adjList2dgCMatrix__,          1},
    {"_gRbase_adjList2ftList__",             (DL_FUNC) &_gRbase_adjList2ftList__,             1},
    {"_gRbase_adjList2ftM__",                (DL_FUNC) &_gRbase_adjList2ftM__,                1},
    {"_gRbase_adjList2matrix__",             (DL_FUNC) &_gRbase_adjList2matrix__,             1},
    {"_gRbase_adjList2tfList__",             (DL_FUNC) &_gRbase_adjList2tfList__,             1},
    {"_gRbase_adjList2tfM__",                (DL_FUNC) &_gRbase_adjList2tfM__,                1},
    {"_gRbase_all_pairs__",                  (DL_FUNC) &_gRbase_all_pairs__,                  4},
    {"_gRbase_allSubsets_",                  (DL_FUNC) &_gRbase_allSubsets_,                  1},
    {"_gRbase_allSubsets0_",                 (DL_FUNC) &_gRbase_allSubsets0_,                 1},
    {"_gRbase_cell2entry_",                  (DL_FUNC) &_gRbase_cell2entry_,                  2},
    {"_gRbase_cell2entry_perm_",             (DL_FUNC) &_gRbase_cell2entry_perm_,             3},
    {"_gRbase_choose_",                      (DL_FUNC) &_gRbase_choose_,                      2},
    {"_gRbase_colmat2list__",                (DL_FUNC) &_gRbase_colmat2list__,                1},
    {"_gRbase_dagList2dgCMatrix__",          (DL_FUNC) &_gRbase_dagList2dgCMatrix__,          2},
    {"_gRbase_dagList2matrix__",             (DL_FUNC) &_gRbase_dagList2matrix__,             2},
    {"_gRbase_dimnames_match_",              (DL_FUNC) &_gRbase_dimnames_match_,              3},
    {"_gRbase_do_combn",                     (DL_FUNC) &_gRbase_do_combn,                     2},
    {"_gRbase_entry2cell_",                  (DL_FUNC) &_gRbase_entry2cell_,                  2},
    {"_gRbase_filter_maximal_vectors_",      (DL_FUNC) &_gRbase_filter_maximal_vectors_,      2},
    {"_gRbase_get_subset_",                  (DL_FUNC) &_gRbase_get_subset_,                  3},
    {"_gRbase_get_superset_",                (DL_FUNC) &_gRbase_get_superset_,                3},
    {"_gRbase_getCliquesDec__",              (DL_FUNC) &_gRbase_getCliquesDec__,              2},
    {"_gRbase_is_dimnames_",                 (DL_FUNC) &_gRbase_is_dimnames_,                 1},
    {"_gRbase_is_named_array_",              (DL_FUNC) &_gRbase_is_named_array_,              1},
    {"_gRbase_is_number_vector_",            (DL_FUNC) &_gRbase_is_number_vector_,            1},
    {"_gRbase_is_subsetof_",                 (DL_FUNC) &_gRbase_is_subsetof_,                 2},
    {"_gRbase_isadjMAT_",                    (DL_FUNC) &_gRbase_isadjMAT_,                    1},
    {"_gRbase_isdagMAT_",                    (DL_FUNC) &_gRbase_isdagMAT_,                    1},
    {"_gRbase_isin_",                        (DL_FUNC) &_gRbase_isin_,                        3},
    {"_gRbase_issymMAT_",                    (DL_FUNC) &_gRbase_issymMAT_,                    1},
    {"_gRbase_isugMAT_",                     (DL_FUNC) &_gRbase_isugMAT_,                     1},
    {"_gRbase_M2dgCMatrix__",                (DL_FUNC) &_gRbase_M2dgCMatrix__,                1},
    {"_gRbase_M2matrix__",                   (DL_FUNC) &_gRbase_M2matrix__,                   1},
    {"_gRbase_make_plevels_",                (DL_FUNC) &_gRbase_make_plevels_,                1},
    {"_gRbase_MAT2ftM_",                     (DL_FUNC) &_gRbase_MAT2ftM_,                     1},
    {"_gRbase_max_set_",                     (DL_FUNC) &_gRbase_max_set_,                     2},
    {"_gRbase_mcsMAT__",                     (DL_FUNC) &_gRbase_mcsMAT__,                     2},
    {"_gRbase_min_set_",                     (DL_FUNC) &_gRbase_min_set_,                     2},
    {"_gRbase_moralizeMAT",                  (DL_FUNC) &_gRbase_moralizeMAT,                  1},
    {"_gRbase_next_cell_",                   (DL_FUNC) &_gRbase_next_cell_,                   2},
    {"_gRbase_next_cell_slice_",             (DL_FUNC) &_gRbase_next_cell_slice_,             3},
    {"_gRbase_next_cell2_",                  (DL_FUNC) &_gRbase_next_cell2_,                  2},
    {"_gRbase_next_perm_",                   (DL_FUNC) &_gRbase_next_perm_,                   1},
    {"_gRbase_order2_",                      (DL_FUNC) &_gRbase_order2_,                      2},
    {"_gRbase_perm_cell_entries_",           (DL_FUNC) &_gRbase_perm_cell_entries_,           2},
    {"_gRbase_RcppExport_registerCCallable", (DL_FUNC) &_gRbase_RcppExport_registerCCallable, 0},
    {"_gRbase_rip_internal",                 (DL_FUNC) &_gRbase_rip_internal,                 3},
    {"_gRbase_rowmat2list__",                (DL_FUNC) &_gRbase_rowmat2list__,                1},
    {"_gRbase_slice2entry_",                 (DL_FUNC) &_gRbase_slice2entry_,                 3},
    {"_gRbase_solveSPD",                     (DL_FUNC) &_gRbase_solveSPD,                     1},
    {"_gRbase_symMAT2ftM_",                  (DL_FUNC) &_gRbase_symMAT2ftM_,                  1},
    {"_gRbase_tab_add_",                     (DL_FUNC) &_gRbase_tab_add_,                     2},
    {"_gRbase_tab_align_",                   (DL_FUNC) &_gRbase_tab_align_,                   2},
    {"_gRbase_tab_div_",                     (DL_FUNC) &_gRbase_tab_div_,                     2},
    {"_gRbase_tab_div0_",                    (DL_FUNC) &_gRbase_tab_div0_,                    2},
    {"_gRbase_tab_equal_",                   (DL_FUNC) &_gRbase_tab_equal_,                   3},
    {"_gRbase_tab_expand_",                  (DL_FUNC) &_gRbase_tab_expand_,                  3},
    {"_gRbase_tab_list_add_",                (DL_FUNC) &_gRbase_tab_list_add_,                1},
    {"_gRbase_tab_list_mult_",               (DL_FUNC) &_gRbase_tab_list_mult_,               1},
    {"_gRbase_tab_marg_",                    (DL_FUNC) &_gRbase_tab_marg_,                    2},
    {"_gRbase_tab_marg2_",                   (DL_FUNC) &_gRbase_tab_marg2_,                   2},
    {"_gRbase_tab_mult_",                    (DL_FUNC) &_gRbase_tab_mult_,                    2},
    {"_gRbase_tab_op_",                      (DL_FUNC) &_gRbase_tab_op_,                      3},
    {"_gRbase_tab_perm_",                    (DL_FUNC) &_gRbase_tab_perm_,                    2},
    {"_gRbase_tab_subt_",                    (DL_FUNC) &_gRbase_tab_subt_,                    2},
    {"_gRbase_tabDiv0__",                    (DL_FUNC) &_gRbase_tabDiv0__,                    2},
    {"_gRbase_tabMarg__",                    (DL_FUNC) &_gRbase_tabMarg__,                    2},
    {"_gRbase_tabMult__",                    (DL_FUNC) &_gRbase_tabMult__,                    2},
    {"_gRbase_topo_sortMAT_",                (DL_FUNC) &_gRbase_topo_sortMAT_,                1},
    {"_gRbase_triang_elo_MAT__",             (DL_FUNC) &_gRbase_triang_elo_MAT__,             2},
    {"_gRbase_triang_mcwh_MAT__",            (DL_FUNC) &_gRbase_triang_mcwh_MAT__,            2},
    {"_gRbase_ugList2dgCMatrix__",           (DL_FUNC) &_gRbase_ugList2dgCMatrix__,           2},
    {"_gRbase_ugList2matrix__",              (DL_FUNC) &_gRbase_ugList2matrix__,              2},
    {"_gRbase_unlist_chr__",                 (DL_FUNC) &_gRbase_unlist_chr__,                 1},
    {"_gRbase_which_",                       (DL_FUNC) &_gRbase_which_,                       1},
    {"_gRbase_which_matrix_index__",         (DL_FUNC) &_gRbase_which_matrix_index__,         1},
    {"_gRbase_which2_",                      (DL_FUNC) &_gRbase_which2_,                      1},
    {"R_colSums",                            (DL_FUNC) &R_colSums,                            1},
    {"R_colwiseProd",                        (DL_FUNC) &R_colwiseProd,                        2},
    {"R_rowSums",                            (DL_FUNC) &R_rowSums,                            1},
    {NULL, NULL, 0}
};

void R_init_gRbase(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
