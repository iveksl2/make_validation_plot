## Hosmer-Lemeshow Test Visualization
[![Build Status](https://travis-ci.org/avantcredit/make_validation_plot.svg?branch=master)](https://travis-ci.org/avantcredit/validationplot) [![Coverage Status](https://coveralls.io/repos/avantcredit/validationplot/badge.svg?branch=master)](https://coveralls.io/r/avantcredit/make_validation_plot)

The performance of any binary classifier that provides a probabilistic
output (rather than a binary value) can be visualized by
splitting the target validation population into quantiles
of a fixed count (usually 10, that is, deciles) and comparing
the empirical incidence of the dependent variable to the
probabilistic response outputted by the classifier.

The validationplot package makes this endeavour straightforward,
especially in conjunction with the \code{tundra} package.

## Installation

This package is not yet available from CRAN (as of October 17, 2015).
To install the latest development builds directly from GitHub, run this instead:

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("avantcredit/make_validation_plot")
library(validationplot)
```

## License

This project is licensed under the MIT License:

Copyright (c) 2015-2016 Avant

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

## Authors

The initial functionality provided in the package was created by physicist Ryland Ely.
Some subsequent maintanenance was provided by Robert Krzyzanowski,
rkrzyzanowski@gmail.com. 


