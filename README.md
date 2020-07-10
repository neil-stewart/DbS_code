# Decision by Sampling Source Code

This source code implements the decision-by-sampling model for risky choices reported in Stewart, Chater, and Brown (2006), Stewart and Simpson (2008) and Stewart (2009).

DbS.R is the library of functions used in the model.

simple_example.R is the example from the on-line calculator. See simple_example.Rout for output.

## Example Function Calls

example_function_calls.R shows how to use all of the key functions in the library. See example_function_calls.Rout for output.

## Kahneman and Tverksky (1979) Choices

Kahneman_Tversky_1979_example.R implements the model as in Stewart and Simpson (2008). Kahneman_Tversky_1979_data.R contains the choices. probs.csv and amounts.csv contain a sample of real-world distributions of amounts and probabilities. See Kahneman_Tversky_1979_example.Rout for output.

## Running the Scripts

The .Rout files show the result of running the .R files. Whole .R scripts can be run from the command line with R CMD BATCH filename.R to create filename.Rout. The scripts were written with R version 2.11.0.

Please send bugs or comments to <Neil.Stewart@warwick.ac.uk>.

