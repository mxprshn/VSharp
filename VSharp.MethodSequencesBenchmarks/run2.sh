#!/bin/bash

# Assign script arguments to variables
DLL_PATH="$1"
KEY="$2"
SEED="$3"
N="$4"
RUNS_DIR="$5"

timestamp=$(date +"%Y-%m-%d_%H-%M-%S")
TARGETS_FILE_NAME="$RUNS_DIR/$timestamp-targets.txt"

dotnet build -c Release

/Users/max/Repos/VSharp/VSharp.MethodSequencesBenchmarks/bin/Release/net7.0/VSharp.MethodSequencesBenchmarks detect "$DLL_PATH" "$KEY" "$SEED" "$N" "$TARGETS_FILE_NAME"

# Initialize an empty array
bench_ids=()

# Read the file line by line
while IFS= read -r line; do
    # Append each line to the array
    bench_ids+=("$line")
done < "$TARGETS_FILE_NAME"

# Use the first N shuffled lines in the array
for bench_id in "${bench_ids[@]}"; do
    echo "Running $bench_id..."
    critical_failures_log="$RUNS_DIR/criticals-$timestamp-$bench_id.log"
    /Users/max/Repos/VSharp/VSharp.MethodSequencesBenchmarks/bin/Release/net7.0/VSharp.MethodSequencesBenchmarks dll-run "$DLL_PATH" "$bench_id" "$KEY" -o "$RUNS_DIR" 2>> "$critical_failures_log"
done
