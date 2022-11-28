Optimal allocation in platform trials
================

This repository contains the main code to reproduce the results of the
paper “Optimal allocations in platform trials” by Marta Bofill Roig,
Ekkehard Glimm, Tobias Mielke, and Martin Posch.

## Derivation of optimal allocations

The following *Mathematical* notebook files are included in this
repository:

-   **case3_b\_lagrange.nb**: Computations on case 3 with Lagrange
    Multipliers. Three-period trial design assuming fixed sample sizes
    in period 1 and 2
-   **case3_ncc.nb**: Computations on case 3 for a three-preiod trial
    using non-concurrent controls.
-   **case2_ncc.nb**: Computations on case 2 using non-concurrent
    controls. Two-period trial design assuming fixed sample sizes in
    period 1 and 2.
-   **case2_lagrange_ncc.nb**: Computations on case 2 using
    non-concurrent controls with Lagrange Multipliers. Two-period trial
    design assuming fixed sample sizes in period 1 and 2, assuming equal
    allocations in period 1 (that is, r01=r11).

To reproduce the plots in the paper, we also included the following R
scripts:

-   **allocation_var_optimize_case3_all.R**: reproduces Figure 2 in the
    paper.
-   **allocation_var_optimize_case3_ncc.R**: to plot Figure 2 in the
    paper but only for optimal allocations for trials with
    non-concurrent controls
-   **allocation_var_optimize_case3.R**: to plot Figure 2 in the paper
    but only for optimal allocations for trials with concurrent controls
    only
-   **allocation_var_optimize_case2_all.R**: reproduces Figure 5 in the
    paper.
-   **allocation_var_optimize_case2_ncc.R**: to plot Figure 5 in the
    paper but only for optimal allocations for trials with
    non-concurrent controls
-   **allocation_var_optimize_case2.R**: to plot Figure 5 in the paper
    but only for optimal allocations for trials with concurrent controls
    only

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

-   **simstudy.R** and **simstudy_timetrends.R** simulate the considered
    trials without and with time trends, respectively.
-   **simresults.R** analyses the simulation results.
-   **casestudy_tables.R** reproduces the tables in the supplementary
    material.
-   **aux_functions.R** includes auxiliar functions to carry out the
    simulations.

## Extra materials

The folder *Slides* contains the code to obtain the plots used for the
presentation at the Conference of the Spanish Region of the
International Biometric Society.
