
# callR.org




## Building dynamic reports

## knitr
```r
# knitr R Markdown to HTML
source('http://callr.org/rfile#001-minimal.Rmd')

# knitr R Markdown to HTML (online)
source('http://callr.org/rfile#https://raw.github.com/yihui/knitr-examples/master/001-minimal.Rmd')
```


## RSP
```r
# RSP to Markdown to HTML report
source('http://callr.org/rfile#LoremIpsum.md.rsp')

# RSP to Markdown to HTML report (online)
source('http://callr.org/rfile#https://raw.github.com/HenrikBengtsson/R.rsp/master/inst/rsp_LoremIpsum/LoremIpsum.md.rsp')

# RSP to Markdown to HTML report (GitHub gist)
source('http://callr.org/rfile#gist://HenrikBengtsson/4c1d81ef18129563c79b/matrixNA-wrong-way.md.rsp')
```


## Sweave
```r
# Sweave to PDF (online)
source('http://callr.org/rfile#https://raw.github.com/HenrikBengtsson/R.rsp/master/inst/rsp_LoremIpsum/LoremIpsum.Rnw')
```


## LaTeX (static)
```r
# TeX to PDF (online)
source('http://callr.org/rfile#https://raw.github.com/HenrikBengtsson/R.rsp/master/inst/rsp_LoremIpsum/LoremIpsum.tex')
```


## Markdown (static)
```r
# Markdown to HTML
source('http://callr.org/rfile#LoremIpsum.md')
```


## Asciidoc (static)
```r
# Asciidoc to PDF (online)
source('http://callr.org/rfile#https://raw.github.com/HenrikBengtsson/R.rsp/master/inst/rsp_LoremIpsum/LoremIpsum.asciidoc.txt')
```

All required packages will automatically be install, if missing.


--------------------------------------------------------------
[install](install.html) | [biocLite](biocLite.html) | [rfile](rfile.html)  

<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-54228632-1', 'auto');
  ga('send', 'pageview');

</script>
