This is the Resubmission of the package 'rrtable'

## Test environments
* local OS X install, R 3.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.


## CRAN submission comment by Swetlana Herbrandt(27-Apr-2018)

Thanks, there is a missing quotation mark in your Description text:
'MS Powerpoint'
	      ^

Please add blanks between
"pdf('LaTex')", "docx('MS Word')" and "pptx('MS powerpoint)".


Please do not comment out your examples and use \donttest{} instead.


Please ensure that your functions (e.g. mycat()) do not write by default or in your examples/vignettes/tests in the user's home filespace. That is not allow by CRAN policies. Please only write/save files if the user has specified a directory. In your examples/vignettes/tests you can write to tempdir().


Please fix and resubmit.

Best,
Swetlana Herbrandt


## Resubmission comment

I have inserted quotation mark in Description text: 'MS Powerpoint'

I have added blanks between 
"pdf('LaTex')", "docx('MS Word')" and "pptx('MS powerpoint)".

I have deleted comment in my examples.
