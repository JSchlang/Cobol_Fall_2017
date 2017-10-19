	   identification division.
	   program-id. program_2.
	   AUTHOR. JOSHUA COLE.
	   		   KELTON ESSIG.
	   		   JOHN SCHLANGER.

	   environment division.
	   input-output section.
	   file-control.
	   		SELECT fileIn assign to 'input.txt'
	   			organization is line sequential.

	   data division.
	   file section.
	   fd fileIn
	   		record contains 33 characters.
	   		data record is employee-in.
	   01 employee-in.
	   		05 socSec	pic 9(9).
	   		05 lastName pic x(13).
	   		05 initials pic xx.
	   		05 hourRate pic 9(5).
	   		05 hourWork pic 9(4).

	   working-storage section.
      * Flag for eof
	   01 data-remains  pic x value 'Y'.

	   procedure division.
	   process-employee-file.
            display 'hello'.
	   		open input employee-in.
	   		read employee-in
	   			at end move 'N' to data-remains
	   		end-read.
	   		stop run.
