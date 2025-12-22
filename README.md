# Visualizing R release dates

Some viz related to R minor and major release dates

## Running on macOS

Depends on the [{fable}](https://fable.tidyverts.org/) package which requires a FORTRAN compiler. If you need one, install [`{macrtools}`](https://mac.thecoatlessprofessor.com/macrtools/) and then use its nice `macrtools::gfortran_install()` function to get that configured.

```{r}
install.packages('macrtools')
macrtools::gfortran_install()
```

