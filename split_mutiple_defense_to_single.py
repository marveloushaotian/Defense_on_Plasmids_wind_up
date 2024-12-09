import pandas as pd
import argparse
from tqdm import tqdm

def process_csv(input_file, output_file):
    # 1. Read the input CSV file
    df = pd.read_csv(input_file)
    
    # 2. Split both Type and Unified_subtype_name columns and expand the dataframe
    df_expanded = df.assign(
        Type=df['Type'].str.split(';'),
        Unified_subtype_name=df['Unified_subtype_name'].str.split(';')
    ).explode(['Type', 'Unified_subtype_name'])
    
    # 3. Save the result to a new CSV file without altering other columns
    df_expanded.to_csv(output_file, index=False)
    print(f"Processed file saved to {output_file}")

def main():
    parser = argparse.ArgumentParser(description='Process CSV file by splitting Type column.')
    parser.add_argument('-i', '--input', required=True, help='Path to input CSV file')
    parser.add_argument('-o', '--output', required=True, help='Path to output CSV file')
    args = parser.parse_args()
    
    print("Starting CSV processing...")
    process_csv(args.input, args.output)
    print("CSV processing completed successfully.")

if __name__ == "__main__":
    main()

# Usage example:
# python script_name.py -i path/to/input.csv -o path/to/output.csv
#
# This script processes a CSV file by splitting the Type column on semicolons,
# creating new rows for each split value, while keeping other columns unchanged.
# The result is saved to a new CSV file.
