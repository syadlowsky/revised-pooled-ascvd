# Clinical implications of revised pooled cohort equations for estimating atherosclerotic cardiovascular disease risk: code for replication

## Updates (5/26/19):
Due to a coding bug extracting the recalibrated coefficients for the intercept and race from the model, Table 2, the Appendix Table, and Figure 4 are inaccurate in the original publication. Attached in `Updated Tables.docx` and `Updated Figure.png` are the updated versions correcting this issue. In addition, the online calculator app has been updated to reflect this change as well. To our best knowledge, all the other numbers reported in the publication are accurate.

![Updated Figure 4](Updated%20Figure.png)

## To run
Use the Stata files to create the cohorts. These should be added to a folder called pooled_data/ in the root directory. From here, use `split_data.R` to create data splits and choose folds for cross validation.

Once you have the data, `fit_model.R` does most of the heavy lifting, including fitting the model to the training data, and running cross validation to get internal validation statistics. `val_model.R` takes this model and runs it on the test data, producing plots and such. `replicate_pces.R` produces the replication tables for the original PCE models. `prop_hazards_check.R` does almost exactly what you'd expect, and `calculate_examples.R` runs the fitted models on the example individuals included in the paper. `pce_baseline.R` produces the results tables for cross validation that are used in the paper.

Most of the rest of the files provide routines for fitting different models or calculating statistics included in the paper.

In moving this from the data center where the work was done, some issues with versions used may pop up. If you find any issues, please email us at basus@stanford.edu


<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.

