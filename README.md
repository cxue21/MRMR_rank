# MRMR_rank
to rank feature selected by MRMR when there is reliability issue with labelling

Code is written in R

The input of the file should be .csv with the first column is the label, and the rests are the radiomics features.

This tool will output some top radiomics features (ranked) which are reliably selected in 20 times of repetition given the ratio of the inconsistent labelling.

Example of the input: 

Enter the file location: /home/cxue21/projectA/Dataset1.csv

Enter the percentage of your expected error rate of labelling (e.g 0.2 to represent 20%): 0.2

Then the code should printout the rank of top radiomics features that are reliably selected.

Happy Coding
