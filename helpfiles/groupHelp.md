---
  title: ""
output: html_document
---
  
  <center> <h4>
  <span style="color:#337ab7"></span>Define high and low groups</h4></center>
  <p></p>
  
  <p>Please select a method from the following options to separate samples into two-groups for survival analysis.</p>
  <li><b>Mean</b>: A mean cutoff is used to define high and low groups</li>
  <li><b>Median</b>: A median cutoff is used to define high and low groups</li>
  <li><b>Percentile</b>: A percentile cutoff is used to define high and low groups.</li></li>
  <ul>
  <li>The <b>upper</b> and <b>lower</b> percentile cutoffs are applied to select for samples with high and low levels, respecively.</li>
  <li>User can drag the slider to pick a value for the upper and lower percentiles.</li>
  <li>The default is set to 0.75 (upper) and 0.25 (lower).</li>
  </ul>
  <li><b>Cutp</b>: A cutpoint is estimated based on martingale residuals using the <i>survMisc</i> package to stratify patients into high and low groups. Note that <b>cutp</b> option takes longer to finish, so click the box next to <b>YES!</b>, if you wish to proceed and please be patient.</li>
  
  <br></br>
  <p>The input data to determine the cutoffs varies with the analysis type: </p>
  <li>RNA-seq normalized FPKM gene expression (<b>single-gene</b> analysis)</li>
  <li>FPKM expression ratio of two genes  (<b>genes ratio</b> analysis)</li>
  <li>Gene Set Enrichment Scores calculated from FPKM expression for a gene set of interest (<b>scRNAseq cluster</b> & <b>gene set</b>)</li>
  <li>Tumor-Infiltrating Lymphoctyes cell proportion (<b>CIBERSOFT TILs</b> analysis) estimates from FPKM expression data using [CIBERSOFT](https://cibersort.stanford.edu/)/li>
  <li>Percentage of Tumor-Infiltrating Lymphocytes (<b>Digital TILs</b> analysis) based on H&E images from [cancer imaging archive](https://wiki.cancerimagingarchive.net/pages/viewpage.action?pageId=33948919)</li>
  <li>gene expression profile scores (<b>profile-based</b> analysis) calculated as the weighted sum of the normalized FPKM  expression values of the gene signature for each tumor</li>
  <li>tumor mutation burden (<b>mutation-based</b> analysis) estimated by the total number of non-synonymous somatic mutations, exonic mutations, and all somatic mutations per Mb for each tumor </li>