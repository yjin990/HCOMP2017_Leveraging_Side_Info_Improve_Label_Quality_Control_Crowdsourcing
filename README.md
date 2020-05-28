Authors: Yuan Jin, Mark Carman, Dongwoo Kim and Lexing Xi

Paper: Leveraging Side Information to Improve Label Quality Control in Crowd-sourcing," Proceedings of the Fifth Conference on
Human Computation and Crowdsourcing, 2017 (HCOMP2017).

Paper Link: https://aaai.org/ocs/index.php/HCOMP/HCOMP17/paper/viewFile/15940/15265

Contact: yuan.jin@monash.edu

This directory contains implementations of side info quality control framework for crowdsourcing using three real-world datasets.

To run the pipeline for training and evaluation on the framwork, simply run the following:

Rscript side_info_qcc_framework.R TREC {labeller info enabled} {item info enabled} {response info enabled} {session info enabled} {#number of burn-in iterations} {#number of item true label samples to keep after burn-in} {#number of responses per labeller} {#number of responses per item}

Command inputs:

labeller info enabled: 0 (disabled) or 1 (enabled)
item info enabled:: 0 (disabled) or 1 (enabled)
response info enabled: 0 (disabled) or 1 (enabled)
session info enabled: 0 (disabled) or 1 (enabled)
#number of burn-in iterations: e.g. 100
#number of item true label samples to keep after burn-in: e.g. 100
#number of responses per labeller: e.g. 50
#number of responses per item: e.g. 3

Example command for running the script on the TREC dataset:

Rscript side_info_qcc_framework.R TREC 1 1 1 1 100 100 54 3
