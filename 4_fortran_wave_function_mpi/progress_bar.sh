#!/bin/bash

# Arguments: $1 = current step, $2 = total steps
current=$1
total=$2
width=50

# Ensure that total is not zero or negative to avoid division by zero
if [ "$total" -le 0 ]; then
    echo "Error: total number of steps must be greater than zero."
    exit 1
fi

# Calculate percentage and bar length
percent=$(( (current * 100) / total ))
bar_length=$(( (percent * width) / 100 ))

# Ensure bar_length is within valid range
if [ "$bar_length" -lt 0 ]; then
    bar_length=0
elif [ "$bar_length" -gt "$width" ]; then
    bar_length="$width"
fi

# Create the bar
bar=$(printf "%${bar_length}s" | tr ' ' '#')
spaces=$(printf "%$((width - bar_length))s")

# Print the progress bar
printf "\r[${bar}${spaces}] ${percent}%% ${current}/${total}"
