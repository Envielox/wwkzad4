#!/bin/bash

cleanup() {
    rm -f socket?
    killall driver > /dev/null 2> /dev/null
    echo "Done."
    exit 0
}

trap cleanup SIGHUP SIGINT SIGTERM

addr[0]="https://portal.invinets.com/latest_reading/update/424BL"
addr[1]="https://portal.invinets.com/latest_reading/update/424EU"
addr[2]="https://portal.invinets.com/latest_reading/update/424GK"
addr[3]="https://portal.invinets.com/latest_reading/update/424FY"


args[0]="0 1 2 3"
args[1]="1 0 2 3"
args[2]="2 0 1 3"
args[3]="3 0 1 2"
args[4]="4 5 6 7"
args[5]="5 4 6 7"
args[6]="6 4 5 7"
args[7]="7 4 5 6"

for (( i = 0; i < 4; i++ )); do
    data=$(curl -s ${addr[$i]})
    stripped_data=$(echo $data | sed -e 's/"[^"]*": //g' | tr -d '{},"')

    echo "Starting process with: " $stripped_data
    data_array=($stripped_data)

    (echo ${data_array[2]} | ./driver ${args[$i]} 2> err${i}.err) &
    sleep 1
    (echo ${data_array[3]} | ./driver ${args[ $i + 4 ]} 2> err${i}.err) &
    sleep 1
done


while [ true ]
do
    sleep 10
done
