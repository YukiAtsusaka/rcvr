# rcvr

### Summary: 
This package helps researchers of ranked-choice voting (RCV) by automating the coding of election identifying variables based on the file name of a cast vote record downloaded from Otis (2025).

### The common problem:
We wish to augment the cast vote records data with additional variables. However, the only identifying information available in the data is typically its file name. 

### The solution:
This package preprocess file names and extract multiple identifying variables. Users can then merge the information.

### Additional benefits
rcvr also constructs a series of election_id, which is directly linked to the archive data (www.archive-rcv.com): a dataset of standardized tabulated results from American ranked-choice voting elections. The package also merges election-level attributes, when users chosoe to do so.




