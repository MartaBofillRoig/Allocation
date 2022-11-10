Optimal allocation in platform trials
================

This repository contains the main code to reproduce the results of the
paper “Optimal allocations in platform trials” by Marta Bofill Roig,
Ekkehard Glimm, Tobias Mielke, and Martin Posch.

## Derivation of optimal allocations

The following scripts are included in this repository:

-   **case3_b\_lagrange.nb**: Computations on case 3 with Lagrange
    Multipliers. Three-period trial design assuming fixed sample sizes
    in period 1 and 2
-   **case2_ncc.nb**: Computations on case 2 using non-concurrent
    controls. Two-period trial design assuming fixed sample sizes in
    period 1 and 2.
-   **case2_lagrange_ncc.nb**: Computations on case 2 using
    non-concurrent controls with Lagrange Multipliers. Two-period trial
    design assuming fixed sample sizes in period 1 and 2, assuming equal
    allocations in period 1 (that is, r01=r11).

## Numerical examples

To run some numerical examples locally, you can install the **shiny**
package in R, and use the function `runGitHub()` to run our shinyapp
`OptiPlat`:

``` r
if (!require('shiny')) install.packages("shiny")
shiny::runGitHub("Allocation", "MartaBofillRoig", subdir = "OptiPlat")
```

## Case study and simulation study

The scripts to reproduce the case study are to be found in the folder
*case-study*.

## Extra materials

The folder *Slides* contains the code to obtain the plots used for the
presentation at the Conference of the Spanish Region of the
International Biometric Society.
