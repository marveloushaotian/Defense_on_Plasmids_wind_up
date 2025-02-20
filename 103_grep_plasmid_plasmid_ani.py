import pandas as pd
from itertools import combinations
from tqdm import tqdm
import os
import argparse
from multiprocessing import Pool, cpu_count
from functools import partial
import numpy as np
from collections import defaultdict

def create_lookup_index(df_b):
    lookup = defaultdict(list)
    
    for idx, row in df_b.iterrows():
        key = tuple(sorted([row['Ref_name'], row['Query_name']]))
        lookup[key].append(idx)
    
    return lookup, df_b

def process_single_file(input_file_a, input_dir, lookup_data, output_dir):
    try:
        lookup, df_b = lookup_data
        
        df_a = pd.read_csv(os.path.join(input_dir, input_file_a))
        
        nuccore_acc_list = df_a['NUCCORE_ACC_updated'].unique()
        pairs = list(combinations(sorted(nuccore_acc_list), 2))
        
        matching_indices = []
        
        for pair in pairs:
            sorted_pair = tuple(sorted(pair))
            if sorted_pair in lookup:
                matching_indices.extend(lookup[sorted_pair])
        
        if matching_indices:
            result_df = df_b.iloc[matching_indices].copy()
            result_df.drop_duplicates(inplace=True)
            
            output_file = os.path.join(output_dir, input_file_a)
            result_df.to_csv(output_file, index=False)
            
            return f"Successfully processed {input_file_a} with {len(result_df)} matches"
        else:
            pd.DataFrame(columns=df_b.columns).to_csv(
                os.path.join(output_dir, input_file_a), 
                index=False
            )
            return f"Processed {input_file_a} - no matches found"
    
    except Exception as e:
        return f"Error processing {input_file_a}: {str(e)}"

def extract_data(input_dir, input_file_b, output_dir, num_processes=None, chunk_size=None):
    if num_processes is None:
        num_processes = max(1, cpu_count() - 1)
    
    os.makedirs(output_dir, exist_ok=True)
    
    print("Reading input file B and creating lookup index...")
    df_b = pd.read_csv(input_file_b)
    lookup_data = create_lookup_index(df_b)
    
    csv_files = [f for f in os.listdir(input_dir) if f.endswith('.csv')]
    
    if chunk_size is None:
        chunk_size = max(1, len(csv_files) // (num_processes * 4))
    
    process_func = partial(process_single_file,
                         input_dir=input_dir,
                         lookup_data=lookup_data,
                         output_dir=output_dir)
    
    print(f"Processing files using {num_processes} processes...")
    with Pool(processes=num_processes) as pool:
        results = list(tqdm(
            pool.imap(process_func, csv_files, chunksize=chunk_size),
            total=len(csv_files),
            desc="Processing files"
        ))
    
    successful = sum(1 for r in results if "Successfully" in r)
    no_matches = sum(1 for r in results if "no matches found" in r)
    errors = sum(1 for r in results if "Error" in r)
    
    print("\nProcessing Summary:")
    print(f"Total files processed: {len(results)}")
    print(f"Successfully processed with matches: {successful}")
    print(f"Processed with no matches: {no_matches}")
    print(f"Errors: {errors}")
    
    print("\nDetailed Results:")
    for result in results:
        print(result)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process plasmid data files using optimized parallel processing.")
    parser.add_argument('-i', '--input_dir', required=True, help='Directory containing input CSV files.')
    parser.add_argument('-b', '--input_file_b', required=True, help='Fixed input CSV file (e.g., all_plasmid_vs_plasmid_skani.csv).')
    parser.add_argument('-o', '--output_dir', required=True, help='Directory to save output CSV files.')
    parser.add_argument('-p', '--processes', type=int, default=None, help='Number of processes to use. Defaults to CPU count - 1.')
    parser.add_argument('-c', '--chunk_size', type=int, default=None, help='Chunk size for parallel processing. Defaults to files/(processes*4).')
    
    args = parser.parse_args()
    
    extract_data(
        args.input_dir,
        args.input_file_b,
        args.output_dir,
        args.processes,
        args.chunk_size
    )