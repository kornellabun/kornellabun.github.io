#!/bin/bash

# Usage: ./gif2webm.sh input.gif [duration_in_seconds]
# Example: ./gif2webm.sh my_portrait.gif 5

INPUT_FILE="$1"
DURATION="$2"
OUTPUT_FILE="${INPUT_FILE%.*}.webm"

if [ -z "$INPUT_FILE" ]; then
    echo "Error: Please provide an input file."
    echo "Usage: ./gif2webm.sh filename.gif [seconds]"
    exit 1
fi

# Build the ffmpeg command
CMD="ffmpeg -i $INPUT_FILE"

# If duration is supplied, add the truncate flag
if [ ! -z "$DURATION" ]; then
    CMD="$CMD -t $DURATION"
fi

# Convert to WebM (VP9 codec), constrained quality (crf 30), no audio (-an)
# -b:v 0 means "variable bitrate" controlled by crf
CMD="$CMD -c:v libvpx-vp9 -b:v 0 -crf 30 -an $OUTPUT_FILE"

echo "Converting $INPUT_FILE to $OUTPUT_FILE..."
$CMD

echo "Done! File saved as $OUTPUT_FILE"
