
# callR.org




## Installing packages

```r
# From CRAN
source('http://callr.org/install#knitr')

# From Bioconductor
source('http://callr.org/install#limma')

# From GitHub
source('http://callr.org/install#rstudio/shiny')
```

All dependent packages will be installed.  If a requested package is already installed and up-to-date, it will not be reinstalled.  All installed packages will be updated.

You can install multiple packages by separating them with commas, e.g.
```r
source('http://callr.org/install#knitr,rstudio/rmarkdown')
```

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

