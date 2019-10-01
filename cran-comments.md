## Resubmission
This is a resubmission.

* Added references to the Description text of
  the DESCRIPTION file
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
  to the format Author (date) <doi:> or <https://>
* Replaced all T/F with TRUE/FALSE

## Test environments
* local OS X install, R 3.6.1
* ubuntu 16.04 (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.
* Possibly mis-spelled words in DESCRIPTION:
    Liu (16:5)
    Reshef (14:18)
    al (14:28, 15:14, 16:12)
    et (14:25, 15:11, 16:9)
    undirected (10:34)
