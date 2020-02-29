# isni 0.4
1. Fix a bug that causes 'NA' in ISNI computation when a clsuter has no observed outcome values. 
 
# isni 1.0
1. Added isniglmmbin() for ISNI computation in GLMM for longitudinal/clustered binary outcomes.
2. Fixed a bug in isnimgm()

# isni 1.2
1. Create factors with explicitly given levels to be consistent with the new default of stringsAsFactors=FALSE in R base. 