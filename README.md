# haskelltalk

![Alt Text](https://s3.amazonaws.com/kukuruku-co/uploads/images/00/00/01/2014/05/29/41b8afd406.png "Haskell interview question")

# Installation

For linux users, I have created some installation scripts.  Simply run ./install\_linux\_deb or ./install\_linux\_rpm.  The scripts install system-level dependencies that are needed for some of the library demos.  The deb script has been tested on debian 10 buster and ubuntu 18.04 bionic; the rpm script should hopefully be fine, but please let me know if you run into any problems. 
Warning: The scripts take 1-2 hours to complete.

The recommended development environment is VSCode with the Haskell Language Server extension (uses hie). The installation script installs VSCode and hie, but you need to install the extension from within VSCode.

Other popular development environments include VSCode + Haskero (uses Intero) or Emacs + Intero. The installation script installs Emacs, and you simply need to append the dotemacs file to your ~/.emacs file.

# Outline

We will try to cover 2-3 modules from the src directory per talk, in roughly the following order.

* Basics.hs
* HigherOrder.hs
* Types.hs
* LazyEvaluation.hs
* Category.hs
* Records.hs
* MutableState.hs
* LambdaCalculus.hs

# Library Demos

* Accelerate.hs
* NumberTheory.hs
* Satisfiability.hs
* Units.hs
* Testing.hs
* Diagrams/Quasifuchsian.lhs
* SizeIndexedVectors.hs

# Other resources

See the references file
