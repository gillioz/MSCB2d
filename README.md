MSCB2d: Momentum-space conformal blocks in 2 dimensions
=======================================================

Description
-----------

This repository contains the [Mathematica](https://www.wolfram.com/mathematica/) package `MSCB2d.m` and some notebooks that make use of it.

The momentum-space conformal blocks have been developed in collaboration with Xiaochuan Lu, Markus Luty, and Guram Mikaberidze, and published at [arXiv:1912.05550](https://arxiv.org/abs/1912.05550) and in the [Journal of High Energy Physics](https://link.springer.com/article/10.1007%2FJHEP03%282020%29102) under the title 'Convergent Momentum-Space OPE and Bootstrap Equations in Conformal Field Theory'.


MSCB2d package
--------------

The package `MSCB2d.m` contains a basic implementation of the momentum-space conformal blocks as described in the publication.

The notebook `MSCB2d_example.nb` gives simple examples on how to use the package.


Free scalar
-----------

The note `Free_scalar.pdf` contains a description of the momentum-space conformal blocks in the theory of a free scalar field, their relation with the ordinary conformal blocks in position space, and examples of 4-point correlation functions that can be expanded into conformal blocks.

The computations are performed in the notebook `Free_scalar.nb`. The animated GIF image `Free_scalar_delta.gif` illustrates how the conformal block expansion approaches a sum of delta functions in the free theory.


Energy-momentum tensors
-----------------------

The notebook `TTTT.nb` computes the expansion of the 4-point correlation function of energy-momentum tensors (T) into momentum-space conformal blocks. This was used to produce figure 4 in the publication.


Ising model
-----------

The notebook `Ising_model.nb` computes the expansion of various 4-point correlation functions of the Ising model into momentum-space conformal blocks.
It makes use of the data contained in the directory `CFTdata` (list of operators and their OPE coefficients).

The notebook contains interactive plots that help visualize the convergence of the OPE (or the absence thereof, depending on the kinematics), as well as the source code for figure 3 in the publication.




