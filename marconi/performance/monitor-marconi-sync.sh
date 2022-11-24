#!/usr/bin/env bash

# For running and monitoring sync testing marconi

if [[ $1 ]]; then marconi_log=$1; else echo "provide marconi log file (arg0)" && exit 1; fi

startMs=$(date +%s)

pid=$(pidof marconi)

if [[ -z $pid ]]
then
	echo -e "pid empty, exiting.\n"
	exit 1
else
	echo -e "marconi pid=$pid\n"
fi

exec > >(tee log.txt)

echo -e "Starting monitoring at $(date)"
echo -e "Process started at $(ps -p "$pid" -o start_time=)\n"
ps -p "$pid" -o etime,%cpu,rss,%mem

while [ true ]
do
	last_log=$(echo $(tail -n1 $marconi_log))
	sync_percent=$(echo $(echo $last_log | cut -d '(' -f2 | cut -d ')' -f1))
	echo -e "Synced: $sync_percent"
	ps -p "$pid" -o etime=,%cpu=,rss=,%mem=

	if [[ "$last_log" == *"Synchronised"* ]] # check for contains "Synchronised"
	then
		echo -e "\nFinished at $(date)"
		echo "Time taken: $(($(date +%s) - $startMs)) seconds"
		exit 1
	else
		sleep 10
	fi
done
