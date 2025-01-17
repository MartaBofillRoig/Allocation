Derivation of optimal allocations
================

The following *Mathematica* notebook files are included in this
repository:

-   **suppmat-CC.nb**: Computation on designs with concurrent controls
    only.
-   **suppmat-NCC-case2.nb**: Computations on case 2 using
    non-concurrent controls.
-   **suppmat-NCC-case3.nb**: Computations on case 3 using
    non-concurrent controls.

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

The R files consider the expressions obtained in the *Mathematica*
notebook files in the folder *mathematica_rcode*. Note that these files
use a different parametrisation. We consider: $r_{i,s} = n_{i,s} /N_s$,
where $n_{i,s}$ is the sample size for arm $i$ in the period $s$, and
$N_s$ is the total sample size in period $s$.

-   **case3_b\_lagrange.nb**: Computations on case 3 with Lagrange
    Multipliers. Three-period trial design assuming fixed sample sizes
    in period 1 and 2
-   **case3_ncc.nb**: Computations on case 3 for a three-period trial
    using non-concurrent controls.
-   **case2_ncc.nb**: Computations on case 2 using non-concurrent
    controls. Two-period trial design assuming fixed sample sizes in
    period 1 and 2.
-   **case2_lagrange_ncc.nb**: Computations on case 2 using
    non-concurrent controls with Lagrange Multipliers. Two-period trial
    design assuming fixed sample sizes in period 1 and 2, assuming equal
    allocations in period 1.

In the folder *comparisons*, we run some examples to check that the solutions using both parameterisations give the same results.