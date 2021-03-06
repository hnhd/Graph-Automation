# Vote Total and Support Graph Automation

In the simplest sense, this script turns raw vote data into a graph. The specific example uses Gubernational election results stratified by county from the 2012 Pence v. Gregg election in Indiana. It's worth noting that this script is very customizable: one can change the type of election (presidential/gubernatorial/etc.), the stratification method (county or DMA), the number of counties or DMA's displayed, and the type of analysis conducted (2-way or 3-way).

This process was established in order to reduce the time spent producing graphs. Instead of copy and pasting already cleaned data into an existing graph and risking all the human errors involved in such a process, this script takes a raw csv file and outputs a graph easily added into a report. Ultimately, this speeds up the process for producing high-level, quality reports. 

There are three documents included in the folder. The script is housed in "template_historical_graph.R", while the data is housed in "in_county_gov_2012.csv". The output is housed in the "images" folder and is named "IN_County_Gubernatorial_2012_3WY". Note that the script automatically produces the "images" folder in its production of the graphic.

In order to run the script ensure that the the working directory is (1) established where the csv is located and (2) that all required packages have been installed. Once both of those have been established, one should be able to run the script and output the image in the images folder. Feel free to adjust the values for the 'width' or 'analysis' variables to see how the final product is changed.
