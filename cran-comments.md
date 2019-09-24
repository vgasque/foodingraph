## Resubmission
This is a resubmission.

* Removed LICENSE file and updated DESCRIPTION
  file from GPL-3 + LICENSE to GPL-3
* Removed \dontrun{} from save_graph.R and
  added tempfile() to save the example in a
  temporary file location
* Removed if(interactive()){} in
  boot_simulated_cat_bin.R
* Replaced print() with message() in
  boot_simulated_cat_bin.R
* Updated boot_simulated_cat_bin.R example to
  run in less than 5s.
* Updated all references in the documentation
  to format Author (date) <doi:> or <https://>
* Replaced all T/F with TRUE/FALSE

## Test environments
* local OS X install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.
* Possibly mis-spelled words in DESCRIPTION:
  undirected (10:34)
