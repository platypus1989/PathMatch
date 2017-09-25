# Path Matching

One of interesting problems in graph theory is the matching of two sets of edges and vertices. The generalized problem, 
which we call the **path-matching** problem, is to pair the vertices of an undirected weighted graph such that the paths 
connecting each pair are subject to certain objectives and/or constraints. One of practical questions that path matching may
enable us to answer is: Given two (vehicle) trip paths with transformed geographical coordinates denoting the locations of vehicle 
at a sequential set of time stamps, how can we make sure they are made by the same driver? Apprarently if the two paths are highly
matched, then most likely they are made by the same driver. 

The question may look easy when we simply visualize the two paths and manually identify the matching parts by bare sight. But 
this approach becomes inpractical when we are confronted with, say, hundreds or even thousands of trips.

![100 Trips entangled together with all the origin points being (0,0))](image/trip_match_plot1.png | width=100)

A path-matching algorithm can help us identify the groups of matched trips.

![Matched trips in 4 groups](image/trip_match_plot2.png | width=100)


## Two-step algorithm

A naive and natural approach to match two trips is to compare the distance and angle of each part of the two trips consecutively and regard it as a match if the differences are smaller than certain threshold. However, because each of the trips are consisted of thousands of points, and the distance between each pair of adjacent points is so small that the cutoff value for matching has to be set as even smaller which make it extremely hard to find a legitimate match. Also in order to find partial match, that is, part (certain percentage) of the trips are matched, we will have to compare the distances and angles in a rolling fashion, it will bring extremely high computational cost.  

The way that we match those trips into groups can be divided into two steps:

1. Simplify the trips into a set of points that best capture the passage information of the trip with the [Ramer–Douglas–Peucker algorithm](https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm), in this way, we shorten the representation of the trips with thousands of points into a set of usually less than 50 points. See the example below.

![Example of RDP algorithm applied to a trip with thousand of points](image/RDP_slow.gif | width=20)

2. We find the matched pairs in the simplified trips with certain cutoffs in distances and angle of each pair of adjacent points. 