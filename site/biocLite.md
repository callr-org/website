<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="author" content="Henrik Bengtsson">
  <link rel="shortcut icon" href="">
  <title>CallR: biocLite</title>
  <link href="http://netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css" rel="stylesheet">
  <link href="assets/css/navbar-fixed-top.css" rel="stylesheet">
  <link href="assets/css/navbar-sticky-bottom.css" rel="stylesheet">
</head>
<body>
<nav class="navbar navbar-default navbar-fixed-top" role="navigation">
  <div class="container-fluid">
    <div class="navbar-header">
      <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      <a class="navbar-brand" href="index.html">CallR</a>
    </div>

    <div class="navbar-collapse collapse">
      <ul class="nav navbar-nav">
        <li><a href="install.html">install</a></li>
        <li><a href="rfile.html">rfile</a></li>
        <li><a href="biocLite.html">biocLite</a></li>
      </ul>
    </div>
  </div>
</nav>

<div id="wrap">

<div class="container">

<h2>Installing Bioconductor packages</h2>

<pre><code class="r">source(&#39;http://callr.org/biocLite#limma&#39;)
</code></pre>

<p>This is a one-line call identical to</p>

<pre><code class="r">source(&#39;http://bioconductor.org/biocLite.R&#39;)
biocLite(&#39;limma&#39;)
</code></pre>

<p>You can install multiple packages by separating them with commas, e.g.</p>

<pre><code class="r">source(&#39;http://callr.org/biocLite#limma,edgeR&#39;)
</code></pre>


</div>

<div id="push"></div>
</div>

<div id="footer" style="height: auto;">
  <div class="container">
<p class="muted credit" style="font-size: small;">
<a href="biocLite">script</a> | 
<a href="http://github.com/callr-org/">GitHub</a>
</p>
  </div>
</div>
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
  <script src="http://netdna.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"></script>
  <script>
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
  
    ga('create', 'UA-54228632-1', 'auto');
    ga('send', 'pageview');
  </script>
</body>
</html>