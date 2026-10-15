# Preprocessing of the data for the supervised learning practical.
#
# Keeps the 1000 most variable genes of the full dataset, to reduce the size of the data and
# the computation time, and saves the result in data-raw : this is the file sent to the students.
#
# Run from the project root (open the .Rproj first).

library(here)
library(dplyr)

# The full dataset : clinical information, the response, then around 10,000 genes (a1cf to zzz3)
df_full <- get(load(here("data-raw", "SDY1119_day7_GE_foldChange.RData")))

n_genes_keep <- 1000

# Names of the genes, and of the other columns (clinical information and response)
gene_names <- colnames(dplyr::select(df_full, a1cf:zzz3))
other_names <- setdiff(colnames(df_full), gene_names)

# Variance of each gene across the participants, and names of the most variable genes
gene_variances <- apply(df_full[, gene_names], 2, var, na.rm = TRUE)
top_genes <- names(sort(gene_variances, decreasing = TRUE))[seq_len(n_genes_keep)]

# Keep the other columns and the most variable genes (in their original, alphabetical order)
df_all <- df_full[, c(other_names, intersect(gene_names, top_genes))]

# Save the filtered dataset
save(df_all, file = here("data-raw", "SDY1119_day7_GE_foldChange_top1000.RData"))

message("Kept ", n_genes_keep, " genes out of ", length(gene_names), " : ",
        nrow(df_all), " participants and ", ncol(df_all), " columns.")
