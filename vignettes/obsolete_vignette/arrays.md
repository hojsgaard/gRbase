%\VignetteEngine{knitr::knitr} 
%\VignetteIndexEntry{arrays: Array operations in gRbase}
%\VignettePackage{gRbase}

\documentclass[10pt]{article}
%\usepackage[T1]{fontenc}
%\usepackage[latin1]{inputenc}
%\usepackage{inputenx}
\usepackage{boxedminipage,color,a4wide,url}
\usepackage[T1]{fontenc}


\def\code#1{\texttt{#1}}
\def\R{\texttt{R}}
\def\pkg#1{\texttt{#1}}

\def\grain{\texttt{gRain}}
\def\grbase{\texttt{gRbase}}
\def\ptab{\code{ptab}}
\def\rr#1{\code{#1}[{\scriptsize gRbase}]}

\usepackage{fancyvrb}

\newlength{\fancyvrbtopsep}
\newlength{\fancyvrbpartopsep}
\makeatletter
\FV@AddToHook{\FV@ListParameterHook}{\topsep=\fancyvrbtopsep\partopsep=\fancyvrbpartopsep}
\makeatother

\setlength{\fancyvrbtopsep}{0pt}
\setlength{\fancyvrbpartopsep}{0pt}


\usepackage{etoolbox} 
\makeatletter 
\preto{\@verbatim}{\topsep=-10pt \partopsep=-10pt } 
\makeatother






\title{Array operations in the \grbase\ package}
\author{S{\o}ren H{\o}jsgaard}
\date{\pkg{gRbase} version 2.0.2 as of 2024-06-03}


\begin{document}
%%\SweaveOpts{concordance=TRUE}

%%\SweaveInput{Rmarkup.STY}

\definecolor{darkred}{rgb}{.7,0,0}
\definecolor{midnightblue}{rgb}{0.098,0.098,0.439}
%% 
%% \DefineVerbatimEnvironment{Sinput}{Verbatim}{
%%   fontfamily=tt,
%%   %%fontseries=b,
%%   %% xleftmargin=2em,
%%   formatcom={\color{midnightblue}}
%% }
%% \DefineVerbatimEnvironment{Soutput}{Verbatim}{
%%   fontfamily=tt,
%%   %%fontseries=b,
%%   %% xleftmargin=2em,
%%   formatcom={\color{darkred}}
%% }
%% \DefineVerbatimEnvironment{Scode}{Verbatim}{
%%   fontfamily=tt,
%%   %%fontseries=b,
%%   %% xleftmargin=2em,
%%   formatcom={\color{blue}}
%% }
%% 
%%\fvset{listparameters={\setlength{\topsep}{-2pt}}}
%%\renewenvironment{Schunk}{\linespread{.95}}{}

\maketitle

\parindent0pt\parskip5pt

\tableofcontents


```
## Error: attempt to use zero-length variable name
```

```r
hec <- c(32, 53, 11, 50, 10, 25, 36, 66, 9, 34, 5, 29) 
dim(hec) <- c(2, 3, 2)
dimnames(hec) <- list(Hair = c("Black", "Brown"), 
                      Eye = c("Brown", "Blue", "Hazel"), 
                      Sex = c("Male", "Female"))
hec
```

```
## , , Sex = Male
## 
##        Eye
## Hair    Brown Blue Hazel
##   Black    32   11    10
##   Brown    53   50    25
## 
## , , Sex = Female
## 
##        Eye
## Hair    Brown Blue Hazel
##   Black    36    9     5
##   Brown    66   34    29
```

Above, \code{hec} is an array because it has a \code{dim} attribute. Moreover,
\code{hec} also has a \code{dimnames} attribute naming the levels of each
dimension. Notice that each dimension is given a name.

Printing arrays can take up a lot of space.  Alternative views on an
array can be obtained with \code{ftable()} or by converting the array
to a dataframe with \code{as.data.frame.table()}. We shall do so in the following.


````r
##flat <- function(x) {ftable(x, row.vars=1)}
flat <- function(x, n=4) {as.data.frame.table(x) %>% head(n)}
hec %>% flat
``` %def 

An array with named dimensions is in this package called a *named array*.
The functionality described below relies heavily on arrays having named dimensions.
A check for an object being a named array is provided by
\rr{is.named.array()}
````

```
## Error: attempt to use zero-length variable name
```

````r
is.named.array(hec)
``` %def 


### Defining arrays


Another way is to use \rr{tabNew()} from \grbase. This function is flexible wrt the input; for example:
````

```
## Error: attempt to use zero-length variable name
```

````r
dn <- list(Hair=c("Black", "Brown"), Eye=~Brown:Blue:Hazel, Sex=~Male:Female)
counts <- c(32, 53, 11, 50, 10, 25, 36, 66, 9, 34, 5, 29)
z3 <- tabNew(~Hair:Eye:Sex, levels=dn, value=counts) 
z4 <- tabNew(c("Hair", "Eye", "Sex"), levels=dn, values=counts)
``` %def 

Notice that the levels list (\code{dn} above) when used in \rr{tabNew()} 
is allowed to contain superfluous elements. Default
\code{dimnames} are generated with
````

```
## Error: attempt to use zero-length variable name
```

````r
z5 <- tabNew(~Hair:Eye:Sex, levels=c(2, 3, 2), values = counts)
dimnames(z5) %>% str
``` %def 


Using \rr{tabNew}, arrays can be normalized to sum to one in two ways:
1) Normalization can be over the first variable for *each*
configuration of all other variables and 2) over all configurations. For
example:
````

```
## Error: attempt to use zero-length variable name
```

````r
z6 <- tabNew(~Hair:Eye:Sex, levels=c(2, 3, 2), values=counts, normalize="first")
z6 %>% flat
``` %def 


## Operations on arrays
{#sec:operations-arrays}

In the following we shall denote the dimnames 
(or variables) of the array \code{hec} by $H$, $E$ and $S$ and we let $(h,e,s)$
denote a configuration of these variables. The contingency table above
shall be denoted by $T_{HES}$ and we shall refer to the
$(h,e,s)$-entry of $T_{HES}$ as $T_{HES}(h,e,s)$. 

### Normalizing an array
{#sec:numarlizing-an-array}

Normalize an array with  \rr{tabNormalize()}
Entries of an array can be normalized to sum to one in two ways:
1) Normalization can be over the first variable for *each*
configuration of all other variables and 2) over all configurations. For
example:
````

```
## Error: attempt to use zero-length variable name
```

````r
tabNormalize(z5, "first") %>% flat
``` %def 


### Subsetting an array -- slicing
{#sec:subsetting-an-array}

We can subset arrays (this will also be called ``slicing'') in
different ways. Notice that the result is not necessarily an
array. Slicing can be done using standard \R\ code or using \rr{tabSlice}.
The virtue of \rr{tabSlice} comes from the flexibility when
specifying the slice:


The following leads from the original $2\times 3 \times 2$
array to a $2 \times 2$
array by cutting away the \code{Sex=Male} and \code{Eye=Brown} slice of the array:
````

```
## Error: attempt to use zero-length variable name
```























































