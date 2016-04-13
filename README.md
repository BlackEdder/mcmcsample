# MCMCSample [![Travis-CI Build Status](https://travis-ci.org/BlackEdder/mcmcsample.svg?branch=master)](https://travis-ci.org/BlackEdder/mcmcsample)

An R package that includes tools to work with posterior samples, e.g. samples
resulting from a MCMC run. It focusses on working on multidimensional samples,
while keeping the dependencies between different parameters intact. In general
it tries to avoid any type of smoothing, but works directly with the actual
samples. It includes functions to plot the different posterior samples,
calculate and plot credible regions and other helper functions. D
