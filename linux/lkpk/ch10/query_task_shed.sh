#!/bin/bash

i=1
printf "  PID       TID            Name                     Sched Policy  Prio *RT  CPU-affinity-mask\n"
prev_pid=1

IFS=$'\n'
for rec in $(ps -LA); do
	[ $i -eq 1 ] && { # skip ps's header
		let i=i+1
		continue
	}

	# 'grok' & extract it, and ...
	pid=$(echo "${rec}" | awk '{print $1}')
	tid=$(echo "${rec}" | awk '{print $2}')
	comm=$(echo "${rec}" | awk '{print $5}')

	rec2=$(chrt -p ${pid} 2>/dev/null)
	rec2_line1=$(echo "${rec2}" | head -n1)
	rec2_line2=$(echo "${rec2}" | tail -n1)
	policy=$(echo ${rec2_line1} | awk -F: '{print $2}')
	prio=$(echo ${rec2_line2} | awk -F: '{print $2}')

	# ... print it!
	[ ${pid} -ne 0 ] && {
		printf "%6d  " ${pid}
		# if it's a child thread ...
		if [ ${pid} -ne 1 -a ${pid} -eq ${prev_pid} ]; then
			printf "  %6d%32s" ${tid} ${comm} # ... indent to the right
		else
			printf "%6d  %32s" ${tid} ${comm}
		fi
		printf "   %12s   %2d" ${policy} ${prio}

		# 'Highlight', with 1 star, any real-time thread, and with 3 stars, those
		# that have an rtprio of 99 !
		rt3=0
		rt1=0
		if [ "${policy}" = " SCHED_RR" -o "${policy}" = " SCHED_FIFO" ]; then
			if [ ${prio} -eq 99 ]; then
				printf "   ***"
				rt3=1
			else
				printf "     *"
				rt1=1
			fi
		fi

		# CPU affinity
		cpuaffinity=$(taskset -p ${pid} 2>/dev/null | cut -d':' -f2)
		if [ ${rt3} -eq 1 -o ${rt1} -eq 1 ]; then
			printf "       ${cpuaffinity}"
		else
			printf "             ${cpuaffinity}"
		fi
		#[ ${rt1} -eq 1 ] && printf "       ${cpuaffinity}"
	}
	printf "\n"
	prev_pid=${pid}
	let i=i+1
done
exit 0
