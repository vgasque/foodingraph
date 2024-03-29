# foodingraph : food network inference

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/foodingraph)](https://cran.r-project.org/package=foodingraph)
[![Travis-CI Build Status](https://travis-ci.org/vgasque/foodingraph.svg?branch=master)](https://travis-ci.org/vgasque/foodingraph)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/vgasque/foodingraph?branch=master&svg=true)](https://ci.appveyor.com/project/vgasque/foodingraph)
[![Downloads](https://cranlogs.r-pkg.org/badges/foodingraph?color=blue)](https://www.r-pkg.org/pkg/foodingraph)

A simple R package to infer food networks from categorical and binary variables.

Displays a weighted undirected graph from an adjacency matrix.
Can perform confidence-interval bootstrap inference with
mutual information or maximal information coefficient.

Based on my Master 1 internship at the Bordeaux Population Health center,
mentored by Boris Hejblum and Cecilia Samieri.

## How it works

From an adjacency matrix, the package can infer the network with confidence-interval (CI) bootstraps of the distribution of mutual information<sup>1</sup> values or maximal information coefficients<sup>2</sup> for each pairwise association.
The CI bootstrap calculated is compared to the CI bootstraps from simulated independent pairwise associations.
The CI bootstrap from simulated independent pairwise variables is used to define a threshold of non-significance in the network. Our approach is to use a threshold for each pairwise variable type : two ordinal variables, two binary variables, one ordinal variable and one ordinal variable.

For example, For each pairwise association, if the 99th percentile of the simulated CI is higher than the 1th percentile of the sample bootstrap distribution, the edge is removed.

From the inferred adjacency matrix, the package can then display the graph using `ggplot2`<sup>3</sup>, `igraph`<sup>4</sup> and `ggraph`<sup>5</sup>.

See R documentation for more information.

## How to use

Two main functions are used to plot graphs : `graph_from_matrix()` and `graph_from_links_nodes()`.
To infer the network, use `boot_cat_bin()`.

For a quick start, see the R vignette `Foodingraph_quickstart`.
For a comprehensive documentation, see the R documentation of the package.

## Authors information

- Victor Gasque (victor.gasque@protonmail.com)
- Boris Hejblum (boris.hejblum@u-bordeaux.fr)
- Cecilia Samieiri (cecilia.samieri@u-bordeaux.fr)

## License

[GNU GPLv3](https://choosealicense.com/licenses/gpl-3.0/)

## References

1: Meyer, Patrick E, Frédéric Lafitte, and Gianluca Bontempi. “Minet: A R/Bioconductor Package for Inferring Large Transcriptional Networks Using Mutual Information.” BMC Bioinformatics 9, no. 1 (December 2008). https://doi.org/10.1186/1471-2105-9-461.

2: Albanese, Davide, Michele Filosi, Roberto Visintainer, Samantha Riccadonna, Giuseppe Jurman, and Cesare Furlanello. “Minerva and Minepy: A C Engine for the MINE Suite and Its R, Python and MATLAB Wrappers.” Bioinformatics 29, no. 3 (February 1, 2013): 407–8. https://doi.org/10.1093/bioinformatics/bts707.

3: H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.

4: Csardi G, Nepusz T: The igraph software package for complex network research, InterJournal, Complex Systems 1695. 2006. http://igraph.org

5: Thomas Lin Pedersen, https://ggraph.data-imaginist.com/
