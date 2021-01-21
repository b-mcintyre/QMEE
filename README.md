# QMEE
Used for QMEE

  The data is a subset of data from a previous experiment conducted in Dr. Ian Dworkin's lab by MSc Caitlyn Daley utilizing D. melanogaster. The data contains crosses of 8 mutant alleles on 2 genes in Oregon-R genetic background with 20 wild type backgrounds from the Drosophila Genetic Research Panel (GDRP). The alleles range from very weak to very severe phenotypic effects on wing phenotype. Severity was measured quantitatively as total area in pixels and semi-quantiatively as morphoicial changes on a scale of 1-21 A score of 1 was given to wings appearing morphologically wild type and 21 given to wings with more severe morphological changes.

The Biological questions I am attempting to answer are if background genetic effects are specific to individual alleles, correlated in alleles of the same gene, or correlated with genes of common phenotypic effects. I would also like to know the variance of background genetic effects between and among strains.

beadex mutant alleles:
bx[3], bx[2], bx[3]

scalloped mutant alleles:
sd[29.1], sd[1], sd[E3], sd[ETX4], sd[58d]  

Oregon-R (ORE) was the background used for beadex and scalloped mutants.

All crosses were reared at 24C in the Percival incubator (RH ~ 60%) on a 12:12 hour day:night cycle (unless otherwise noted).

CD_Wing_Macro:

// Macro to compute wing area for Drosophila images.
macro "WingSizeMacro2 [q]" {
run("Scale...", "x=0.25 y=0.25 width=1020 height=768 interpolation=Bilinear average create");
run("Find Edges");
run("Enhance Contrast...", "saturated=0.4");
run("Sharpen");
run("Sharpen");
run("Make Binary");
run("Close-");
run("Dilate");
run("Fill Holes");
run("Despeckle");
run("Remove Outliers...", "radius=50 threshold=50 which=Dark");
run("Analyze Particles...", "size=50-Infinity pixel display summarize");
// run("Close");
//
