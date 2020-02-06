
<!-- README.md is generated from README.Rmd. Please edit that file -->

# STRINGDatabaseManipulation

The goal of STRINGDatabaseManipulation is to provide functions for
working with STRING protein-protein interaction networks.

## Installation

Only the development version of STRINGDatabaseManipulation is available.
You will also need the `graph` package from Bioconductor.

``` r
install.packages("BiocManager")
BiocManager::install("graph")
remotes::install_github("rmflight/STRINGDatabaseManipulation")
```

## Issues

If you have a question about how to use the package, a request for
something new to be implemented, or have a bug, please [file an
issue](https://github.com/rmflight/STRINGDatabaseManipulation/issues).

## Getting Data

This package works with preprocessed data from STRING itself.

You can preprocess files from STRING by downloading detail and alias
files.

For example, for human you can download the detail file by:

    # get the PPI network itself
    wget https://stringdb-static.org/download/protein.links.detailed.v11.0/9606.protein.links.detailed.v11.0.txt.gz
    # get aliases
    wget https://stringdb-static.org/download/protein.aliases.v11.0/9606.protein.aliases.v11.0.txt.gz

Note: these links become available after filtering by first choosing an
organism.

And then process them so they are easier to use:

``` r
library(STRINGDatabaseManipulation)
# assuming data is in the file, you would do:
ppi_data = process_string_data("9606.protein.links.detailed.v11.0.txt.gz")

# save it for later
saveRDS(ppi_data, file = "tmp_ppi_data.rda")

# process aliases
protein_aliases = process_string_id("9606.protein.aliases.v11.0.txt.gz")

# save for later
saveRDS(protein_aliases, file = "tmp_aliases.rda")
```

I have smaller versions of the data files saved for examples.

``` r
library(STRINGDatabaseManipulation)
data("STRING11_9606_links")
head(STRING11_9606_links)
#>                     protein1             protein2 neighborhood fusion
#> 233571  9606.ENSP00000255305 9606.ENSP00000261600            0      0
#> 1373383 9606.ENSP00000368976 9606.ENSP00000480987            0      0
#> 1920853 9606.ENSP00000472998 9606.ENSP00000362820            0      0
#> 1919582 9606.ENSP00000472836 9606.ENSP00000481697            0      0
#> 159715  9606.ENSP00000242104 9606.ENSP00000348772            0      0
#> 1086730 9606.ENSP00000354558 9606.ENSP00000234256            0      0
#>         cooccurence coexpression experimental database textmining
#> 233571            0           49          812        0         49
#> 1373383           0          430            0        0         42
#> 1920853           0          737          683      900        225
#> 1919582           0           53          413        0          0
#> 159715            0           62            0        0        767
#> 1086730           0            0            0        0        565
#>         combined_score
#> 233571             815
#> 1373383            430
#> 1920853            992
#> 1919582            420
#> 159715             772
#> 1086730            565

data("STRING11_9606_aliases")
head(STRING11_9606_aliases)
#>                       string           other
#> 655068  9606.ENSP00000350937      EAL24365.1
#> 1813946 9606.ENSP00000356015          Q5TCL7
#> 2034111 9606.ENSP00000262320   UPI000012669E
#> 58501   9606.ENSP00000318585            5DQC
#> 874028  9606.ENSP00000374036 ENST00000318198
#> 479329  9606.ENSP00000478474          C9J212
#>                                                                                                                type
#> 655068                                                                                           Ensembl_protein_id
#> 1813946 BLAST_UniProt_AC Ensembl_HGNC_UniProt_ID(supplied_by_UniProt)_AC Ensembl_UniProt_AC Ensembl_UniProt_synonym
#> 2034111                                                                                             Ensembl_UniParc
#> 58501               BLAST_UniProt_DR_PDB Ensembl_HGNC_UniProt_ID(supplied_by_UniProt)_DR_PDB Ensembl_UniProt_DR_PDB
#> 874028                                                                                   Ensembl_archive_transcript
#> 479329          BLAST_UniProt_AC Ensembl_HGNC_UniProt_ID(supplied_by_UniProt)_AC Ensembl_UniProt Ensembl_UniProt_AC
```

## Using Data

Now lets actually do something with the STRING data. Lets find all nodes
within so many hops (3) of a set of starting nodes.

We will use the example data from the package.

``` r
library(graph)
ppi_data = STRING11_9606_links

# we also trim it to a random 10000 to make this example tractable
ppi_graph = string_2_graphBAM(ppi_data)
start_nodes = sample(nodes(ppi_graph), 10)
n_hop = 3

after_3 = find_edges(ppi_graph, start_nodes, n_hop)
after_3$graph
#> A graphBAM graph with undirected edges
#> Number of Nodes = 257 
#> Number of Edges = 259

after_32 = find_edges(ppi_graph, start_nodes, n_hop, drop_same_after_2 = FALSE)
after_32$graph
#> A graphBAM graph with undirected edges
#> Number of Nodes = 257 
#> Number of Edges = 259
```

In this case nothing changed, but depending on the inputs, it might.

## Visualizing

If you want to visualize the results, you should be able to convert the
`graphBAM` object to something that `ggraph` can plot.

## Code of Conduct

Please note that the ‘STRINGDatabaseManipulation’ project is released
with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
