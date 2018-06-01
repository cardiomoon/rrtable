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
"pdf('LaTex')", "docx('MS Word')" and "pptx('MS powerpoint')".

I have deleted comments in my examples.

Now, my functions write files in tempdir() by default.

Thank you very much.


## CRAN submission comment by Uwe Ligges(29-May-2018)

But in your examples you still have code such as

require(rrtable)
require(officer)
require(magrittr)
title="Two Tables"
ft1=df2flextable(head(iris[1:4]))
ft2=df2flextable(tail(iris[1:4]))
doc=read_docx()
doc \%>\% add_text(title=title) \%>\%
       add_2flextables(ft1,ft2) \%>\%
       print(target="2tables.docx")

that write to the current directory, i.e. the user filespace which is not permitted. Use tempdir() instead (also in examples and tests).

Also, please specify version dependencies on the other packages as your package dailed to install on a rather outdated system.

## Resubmission comment

I have rewritten all the examples. 

I have added specific versions of packages in my description.

Thank you very much.
