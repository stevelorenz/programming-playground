$2 > 6 { n = n + 1; pay = pay + $2 * $3 }
END {
	if (n > 0) 
		print n, "employees are paid more than $6/hour"
	else
		print "no employees are paid more than $6/hour"
	}
