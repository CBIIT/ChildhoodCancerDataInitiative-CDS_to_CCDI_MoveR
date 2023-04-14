# ChildhoodCancerDataInitiative-CDS_to_CCDI_MoveR
This script will take a CDS metadata manifest file (flat) and move the contents to a CCDI metadata manifest (tabular).

To run the script please obtain a [CCDI Submission Template](https://github.com/CBIIT/ccdi-model/tree/main/metadata-manifest) and, run the following command in a terminal where R is installed for help.

```
Rscript --vanilla CCDI-CDS_to_CCDI_MoveR.R -h
```

```
Usage: CCDI-CDS_to_CCDI_MoveR.R [options]

CCDI-CDS_to_CCDI_MoveR v1.0.0

Options:
	-f CHARACTER, --file=CHARACTER
		dataset file, CDS_submission_metadata_template.xlsx

	-t CHARACTER, --template=CHARACTER
		dataset template file, CCDI_submission_metadata_template.xlsx

	-h, --help
		Show this help message and exit
```
