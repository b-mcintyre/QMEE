# QMEE
Used for QMEE

Exercise 3
Main file to look at called: "GGboxplots" found in b-mcintyre/QMEE in the main branch
data taken from data set titled "NEW_CD_DGRO_Subset_Data_2019_V2"

In my boxplots I am trying to show two related points the first two boxplots show the variation between mutant 
phenotypes regardless of the genetic backgrounds for both the total area as well as the semi-quantitative scale.
The third and fourth boxplots show the variation within mutant phenotypes between different genetic backgrounds
for both the total area as well as the semi-quantitative scale. 
I chose to display this information in boxplot format as I had a medium data set size and I believed that boxplots
would allow for a good understanding of this variation through observing the lengths of the boxes and the min and max values. In other words, the longer the boxplot the more variable the mutation is when comparing irrespective of backgrounds as well as how variable a mutation is between backgrounds. Furthermore, I believed this choice would give information of the distribution of the mutational effects within the different backgrounds depending on the spread of the quartiles. 
The Cleaveland hierarchy lead me to these choices. I felt that comparing positions along a common scale(via semi-quantitative or total area measurements) and comparing the lengths of the boxes and their min/maximums on this scale would provide a clear picture of the variation of different mutations.

Exercise 2
Main file to look at called: "means_SQ_TA" found in b-mcintyre/QMEE in the main branch
data taken from data set titled "NEW_CD_DGRO_Subset_Data_2019_V2"

Main investigations include ensuring there is no duplicates in the total areas measurements as it was done by a macro
implying that it is very unlikely a wing measure will be the same. If there are found duplicates I must got back to the
raw image data and re-examine them to determine if they are duplicates.
Seeing how many replicates were completed and ensuring that at least 2 replicates were completed among genotypes.
Looking into the replicates to see how many there were and what they were. 
Cleaning the data by created a genotype table, converting the pixel measurement into a mmsqr measurement, and creating
a averages table for the semi-quantitative and total wing area in mmsqr based on genotype.
I intend to use this table to conduct comparisons on variability inter-genetically on both a quantitative and 
semi-quantitative level, as well as use the means to compare alleles of similar phenotypic effects. 
To allow for reproducibility I broke my code into logical steps utilizing comments such as data checking and cleaning, 
I placed comments above the code to state what I was doing, I titled my code in the script, I included the R version my
code was created on in the script, I pipe-lined my code for the ability to easily break and edit my code, and I placed 
the raw data utilized on github with this README file I stating where to find the code.  

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
