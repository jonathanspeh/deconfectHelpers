# import cellanneal as ca
# import pandas as pd
# 
# 
# def run_cellanneal(signature, mixture, disp_min = 0.5, bulk_min = 1e-5, bulk_max = 0.01, maxiter = 1000):
#   maxiter = int(maxiter)
#   signature_df = pd.read_csv(signature, index_col = 0)
#   mixture_df = pd.read_csv(mixture, index_col = 0)
#   gene_dict = ca.make_gene_dictionary(
#                     signature_df,
#                     mixture_df,
#                     disp_min=disp_min,
#                     bulk_min=bulk_min,
#                     bulk_max=bulk_max
#                     )
#   all_mix_df = ca.deconvolve(
#                   signature_df,
#                   mixture_df,
#                   maxiter=maxiter,
#                   gene_dict=gene_dict
#                   )
#   return all_mix_df
# 
# 
# 
