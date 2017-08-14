Self-Organizing Map
--------------------
[Self-Organizing Maps (SOM, Kohonen Map)](https://en.wikipedia.org/wiki/Self-organizing_map) 
written by *Teuvo Kohonen* in 1980s. It is a type of artificial neural 
network (ANN) that is trained using unsupervised learning to produce a
low-dimensional (typically two-dimensional), discretized representation of the
input space of the training samples.

The conversion of a high-dimensional space into low-dimensional output makes 
SOM lucrative and interesting for visualization. Such visualisation makes even 
high-dimensional problems understandable and relatively easy to imagine.  

User guide
----------
Click on `Import data` and select your data set. Once data are loaded
succesfully, we can pick features which you are interested in (tab `Profiling`,
within data table there is column `use`). After this, data will undergo several
validity checks and all
[categorical](https://stat.ethz.ch/R-manual/R-devel/library/base/html/factor.html) variables will be re-written into dummies variables. This is done due to SOM'
inability to work with categorical features.


Now we need to start SOM training (`SOM/Train SOM`). Here, multiple options 
might be set and tuned. Check [Kohonen package](https://cran.r-project.org/web/packages/kohonen/kohonen.pdf) 
for more information. Whenever you change some setting, click *Train SOM* 
button and it will be re-trained. All training aspects and features can be
observed on the right side tab (*fan*, *changes*, *counts*, *quality*, *dist* 
and *codes*). 


Once we are satisfied with the training, we can proceed to clustering of our
data (`SOM/Clustering`). Similarly to the training tab, also here, 
multiple settings are avaiable and after clicking on `Cluster SOM` button 
our data are cluctered based on these settings.

Finaly, in the `Download` tab all results can be downloaded as a zip file.

