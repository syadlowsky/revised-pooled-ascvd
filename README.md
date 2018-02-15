# Clinical implications of revised pooled cohort equations for estimating atherosclerotic cardiovascular disease risk: code for replication

## To run
Use the Stata files to create the cohorts. These should be added to a folder called pooled_data/ in the root directory. From here, use `split_data.R` to create data splits and choose folds for cross validation.

Once you have the data, `fit_model.R` does most of the heavy lifting, including fitting the model to the training data, and running cross validation to get internal validation statistics. `val_model.R` takes this model and runs it on the test data, producing plots and such. `replicate_pces.R` produces the replication tables for the original PCE models. `prop_hazards_check.R` does almost exactly what you'd expect, and `calculate_examples.R` runs the fitted models on the example individuals included in the paper. `pce_baseline.R` produces the results tables for cross validation that are used in the paper.

Most of the rest of the files provide routines for fitting different models or calculating statistics included in the paper.

In moving this from the data center where the work was done, some issues with versions used may pop up. If you find any issues, please email me at syadlows otter stanford.edu, replacing any water loving mammals with an @ symbol.
