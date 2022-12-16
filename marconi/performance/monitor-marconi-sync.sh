#!/usr/bin/env bash

# For monitoring time to sync marconi. Logs cpu, mem and sync %.

# Run marconi with "> >(tee marconi-log.txt)" and then run this script with marconi's log as arg
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

exec > >(tee monitor-marconi-log.txt)

echo -e "Starting monitoring at $(date)"
echo -e "Process started at $(ps -p "$pid" -o start_time=)\n"
ps -p "$pid" -o etime,%cpu,rss,%mem

while true
do
	last_log=$(tail -n1 "$marconi_log")
	sync_percent=$(echo "$last_log" | cut -d '(' -f2 | cut -d ')' -f1)
	echo -e "Synced: $sync_percent"
	ps -p "$pid" -o etime=,%cpu=,rss=,%mem=

	if [[ "$last_log" == *"Synchronised"* ]]
	then
		echo -e "\nFinished at $(date)"
		echo "Time taken: $(($(date +%s) - startMs)) seconds"
		exit 1
	else
		sleep 10
	fi
done
