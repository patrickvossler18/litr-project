
<!-- README.md is generated from README.Rmd. Please edit that file -->

# litr: Writing R packages via literate programming <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

## Overview

The `litr` R package lets you write a complete R package in a single R
markdown document. This enables a workflow for writing R packages that
is probably very different from what you are used to.

<figure>
<img src="man/figures/diagram3.png"
alt="With litr, knitting creates an R package in addition to the .html file." />
<figcaption aria-hidden="true"><em>With litr, knitting creates an R
package in addition to the .html file.</em></figcaption>
</figure>

## Why write R packages in this way?

Using `litr` brings all the benefits of R markdown to package
development:

- **Record and explain** every step of making your R package so that you
  and others can understand every detail years later.
- Present the functions in your package in a logical order that
  maximizes **human readability**.
- Include all the derivations in **latex right next to the code** that
  ends up in your package.
- Include **figures** alongside code to help in the explanation.
- Define **unit tests in the relevant context**, i.e. directly after
  defining the function to be tested.

Furthermore, writing your R package is actually *easier* with `litr`
than without it. Just choose one of the R package
[templates](https://jacobbien.github.io/litr-project/articles/package-templates.html)
below and press “Knit” – and you’ll have a working R package that you
can then modify.

## Installation

You can install the latest release of `litr` on github with the
following:

``` r
remotes::install_github("jacobbien/litr-project@*release", subdir = "litr")
```

Or for the latest development version, remove the `@*release` in the
above.

## Getting started

Using a template is the best way to get started:

``` r
rmarkdown::draft("create-rhello.Rmd", template = "make-an-r-package", package = "litr")
```

This creates an R markdown file called
[`create-rhello.Rmd`](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package/create-rhello.Rmd)
that demonstrates the literate programming workflow for writing an R
package. In particular, when you knit `create-rhello.Rmd`, it creates a
tiny example R package called
[`rhello`](https://github.com/jacobbien/litr-project/tree/main/examples/make-an-r-package/rhello)
with one function and one test function. To knit, you can either press
“Knit” in RStudio or use the following command:

``` r
litr::render("create-rhello.Rmd")
```

This creates an R package! Now you can modify the template to design
your own package.

To explore other kinds of R packages, such as those using `Rcpp`, see
the
[templates](https://jacobbien.github.io/litr-project/articles/package-templates.html)
page. Also, see the section on [packages in the
wild](https://jacobbien.github.io/litr-project/articles/packages-in-the-wild.html)
that use `litr`.

## More background

When you try to understand the code in an R package, the logic of how
the functions relate to each other is often not obvious. While including
function documentation, vignettes, and unit tests are all best
practices, they do not convey the chain of logic in the mind of the
programmer that went into writing the different functions. It can be
difficult to know how to look through all the functions within even a
well-documented R package. The fact that the functions appear in
different files and that functions within files can be defined in
arbitrary order makes it unclear how to approach reading the code.
Furthermore, tests are stored in a different place from the functions
themselves, making the tests harder to read. This would all be resolved
if we could have a single document that goes through all code and tests
in a linear, logical fashion. Rather than try to construct such a
document after the fact, the idea of `litr` is to *make this document
the actual source code for the package*. **The R package is created
through the act of knitting this document.** If we want to modify
anything in the package, then we do so by modifying this document and
re-knitting.

The above motivation is that of [literate
programming](https://en.wikipedia.org/wiki/Literate_programming),
introduced by [Donald
Knuth](https://www-cs-faculty.stanford.edu/~knuth/), and the direct
inspiration is [fast.ai](https://www.fast.ai/about/)’s
[`nbdev`](https://nbdev.fast.ai/), which is available in Python. The
`litr` package relies heavily on a number of great tools in R,
especially [`knitr`](https://yihui.org/knitr/),
[`rmarkdown`](https://rmarkdown.rstudio.com/docs/index.html),
[`usethis`](https://usethis.r-lib.org/),
[`devtools`](https://devtools.r-lib.org/), and
[`testthat`](https://testthat.r-lib.org/). I should also note that
[Yihui Xie](https://yihui.org/en/) has [a post](https://yihui.org/rlp/)
where he demonstrates a similar idea, although his approach appears to
be more of a proof of concept.

You can hear [Jeremy
Howard](https://www.fast.ai/about.html#jeremy-howard) and [Hugo
Bowne-Anderson](https://hugobowne.github.io/) talk about literate
programming and [`nbdev`](https://nbdev.fast.ai/) on the [Vanishing
Gradients podcast](https://vanishinggradients.fireside.fm/2). This
discussion is what inspired `litr`!
