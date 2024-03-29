
/* C Header */

/*
    Copyright (C) 2017-2019 Torsten Hothorn

    This file is part of the 'libcoin' R add-on package.

    'libcoin' is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, version 2.

    'libcoin' is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with 'libcoin'.  If not, see <http://www.gnu.org/licenses/>.


    DO NOT EDIT THIS FILE

    Edit 'libcoin.w' and run 'nuweb -r libcoin.w'
*/

/* R Includes */

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <R_ext/stats_package.h> /* for S_rcont2 */
#include <R_ext/Applic.h> /* for dgemm */
#include <R_ext/Lapack.h> /* for dgesdd */

/* C Macros */

#define S(i, j, n) ((i) >= (j) ? (n) * (j) + (i) - (j) * ((j) + 1) / 2 : (n) * (i) + (j) - (i) * ((i) + 1) / 2)
#define LE(x, y, tol) ((x) < (y)) || (fabs((x) - (y)) < (tol))
#define GE(x, y, tol) ((x) > (y)) || (fabs((x) - (y)) < (tol))

/* C Global Variables */

#define ALTERNATIVE_twosided                            1
#define ALTERNATIVE_less                                2
#define ALTERNATIVE_greater                             3

#define TESTSTAT_maximum                                1
#define TESTSTAT_quadratic                              2

#define LinearStatistic_SLOT                            0
#define Expectation_SLOT                                1
#define Covariance_SLOT                                 2
#define Variance_SLOT                                   3
#define ExpectationX_SLOT                               4
#define varonly_SLOT                                    5
#define dim_SLOT                                        6
#define ExpectationInfluence_SLOT                       7
#define CovarianceInfluence_SLOT                        8
#define VarianceInfluence_SLOT                          9
#define Xfactor_SLOT                                    10
#define tol_SLOT                                        11
#define PermutedLinearStatistic_SLOT                    12
#define StandardisedPermutedLinearStatistic_SLOT        13
#define TableBlock_SLOT                                 14
#define Sumweights_SLOT                                 15
#define Table_SLOT                                      16

#define DoSymmetric                                     1
#define DoCenter                                        1
#define DoVarOnly                                       1
#define Power1                                          1
#define Power2                                          2
#define Offset0                                         0
