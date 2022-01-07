---
title: ""
output: html_document
---

<center> <h4>
<span style="color:#337ab7">Input Cluster File Format</span></h4> </center>

<br>
</br>
<h4>
An example single-cell RNA-seq cluster 'txt' file:
</h4>
![](clusterFile.png)

<br>
</br>
<h4>In the input file, there must be a column for,</h4> 
<p>----------------------------</p>
<li>Cluster, labeled as <b>"cluster"</b></li>
<li>Human HGNC gene symbols, labeled as <b>"gene"</b></li>
<li>FDR, column labeled as <b>"p_val_adj"</b></li>
<li>Any other columns are optional</b></li>
<li>The order of the columns does not matter</b></li>
