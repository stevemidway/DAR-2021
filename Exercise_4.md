Exercise 4: Plotting Basics
================
DAR Lab
Spring 2021

1.  Create a line plot of the `LakeHuron` data set. Time (index) should
    be on the *x*-axis and lake level on the *y*-axis. Consider scaling
    the *y*-axis and labeling the *x*-axis. Show the code and the
    output.

2.  Using the `swiss` dataset, create a barplot showing the `Fertility`
    of the 5 highest and 5 smallest values. Show the code and the
    output. (Hint `dplyr::arrange()` may help, but there are multiple
    ways to do this.) It is OK if the location labels are not included.
    Show the code and output.

3.  Tabulate the `women` dataset, but do so using the table syntax in
    Markdown, not the `table()` function in R.

4.  Using the `Orange` dataset and the `library(lattice)`, create a
    paneled figure where each individual `Tree` has its own panel.
    Within each panel, plot `circumference` as a function of `age`. No
    model (line) is required; simply plot the data as a scatterplot. You
    may enhance the figure anyway you would like (e.g., point sizes,
    colors, labels, etc.)

5.  Simulate 4 normal distributions each with a mean of 0 and with
    standard deviation of 1, but with sample sizes of 10, 25, 100, and
    500. Using `mfrow()` or `mfcol()` panel histograms of these
    distributions and label which panels contain which information.
    Scale the axes to be the same for all panels, so that relative
    magnitude of the distributions can be visually interpreted.

**Please complete the questions in an R Markdown file. Include the
questions by replicating the format in this document. [Email
me](smidway@lsu.edu) the html version of your completed assignment by
5pm Tuesday, February 16.**
