import pandas as pd
import argparse
from tqdm import tqdm

def process_csv(input_file, output_file):
    df = pd.read_csv(input_file)
    
    df_expanded = df.assign(
        Type=df['Type'].str.split(';'),
        Unified_subtype_name=df['Unified_subtype_name'].str.split(';')
    ).explode(['Type', 'Unified_subtype_name'])
    
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
