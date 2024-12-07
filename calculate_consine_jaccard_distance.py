import pandas as pd
import numpy as np

def cosine_similarity(list1, list2, unique_elements):
    """Calculate cosine similarity between two lists"""
    vector_1 = np.zeros(len(unique_elements))
    vector_2 = np.zeros(len(unique_elements))
    
    for element in list1:
        if element in unique_elements:
            vector_1[unique_elements == element] += 1
    
    for element in list2:
        if element in unique_elements:
            vector_2[unique_elements == element] += 1
    
    # Avoid division by zero
    norm_product = np.linalg.norm(vector_1) * np.linalg.norm(vector_2)
    if norm_product == 0:
        return 0
    
    cosine_similarity = np.dot(vector_1, vector_2) / norm_product
    return cosine_similarity

def jaccard_distance(list1, list2):
    """Calculate Jaccard distance between two lists"""
    # Convert lists to sets
    set1 = set(list1)
    set2 = set(list2)
    
    # Calculate intersection and union
    intersection = set1 & set2
    union = set1 | set2
    
    # Avoid division by zero
    if len(union) == 0:
        return 0
    
    return 1 - len(intersection) / len(union)

# Read the input file
input_file = 'master_table_pcn_with_ptu.csv'  # 替换为你的输入文件名
master_table = pd.read_csv(input_file)

# Ensure Unified_subtype_name column is string and handle missing values
master_table["Unified_subtype_name"] = master_table["Unified_subtype_name"].fillna("").astype(str)

# Initialize list to store passed PTUs
passed_ptus = []

# Get unique elements for cosine similarity calculation
unique_elements = np.array(master_table['Unified_subtype_name'].unique())

# Process each PTU group
for ptu_name, ptu_group in master_table.groupby("PTU_sHSBM (10)"):
    # Check if the group meets the criteria
    if len(ptu_group) > 10 and len(ptu_group[ptu_group["num_def_sys"] > 0])/len(ptu_group) > 0.1:
        passed_ptus.append(ptu_name)
        
        # Get vectors for the rows within the group
        vectors = [str(x) for x in ptu_group["Unified_subtype_name"].tolist()]
        
        # Initialize matrices
        size = len(vectors)
        matrix_cosine = np.zeros((size, size))
        matrix_jaccard = np.zeros((size, size))
        
        # Compare each pair without redundancy
        for i in range(size):
            for j in range(i + 1, size):
                # Calculate similarities
                cosine_sim = cosine_similarity(vectors[i].split(';'), vectors[j].split(';'), unique_elements)
                jaccard_dist = jaccard_distance(vectors[i].split(';'), vectors[j].split(';'))
                
                # Store values symmetrically
                matrix_cosine[i, j] = cosine_sim
                matrix_cosine[j, i] = cosine_sim
                matrix_jaccard[i, j] = jaccard_dist
                matrix_jaccard[j, i] = jaccard_dist
        
        # Save matrices as CSV files
        cosine_df = pd.DataFrame(matrix_cosine)
        jaccard_df = pd.DataFrame(matrix_jaccard)
        
        cosine_df.to_csv(f'cosine_matrix_{ptu_name}.csv', index=False)
        jaccard_df.to_csv(f'jaccard_matrix_{ptu_name}.csv', index=False)

# Print summary
print(f"Analysis completed for {len(passed_ptus)} PTUs")
print("Passed PTUs:", passed_ptus)
