### News in Prostar 1.28

#### New features
* In the 'Hypothesis testing' tool, the interface for the 'Swap conditions' option has been updated (a collapsible panel has been added).
* Textual information has been added in the imputation tool when the dataset contains no missing values. This prevents any user to run an imputation on such datasets (which made some imputation methods crash)

#### Bugs fixed
* [1.28.0]
  * The 'Reset' action button in 'Differential Analysis' has been fixed. Now, the 'Push p-value' widgets are also set back to default values.
  * Bug fixed when exporting a peptide dataset as an Excel file
  * The 'Reload Prostar' button works now (Prostar used to crash when the user clicked on the button)
  * Bug fixed in 'Normalization' tool when normalizing with tags in a 'Specific column'
  * Bug fixed with the Push p-value feature in 'Differential analysis' tool.
  * Prostar can now handle peptide datasets in which the protein accession contains "|" (The problem occurred during  connected component computation)
  * Bug fixed about the display of quantitative metadata in the data explorer (the columns and their names were not aligned)
  * Bug fixed about Prostar freeze when saving the aggregated dataset.
