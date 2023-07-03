import pandas as pd

def clean_data(csv_path):
    network = pd.read_csv(csv_path, na_filter=True)
    network['Children'] = network['Children'].apply(lambda x: x.replace(', ', '","').replace('[', '["').replace(']', '"]'))
    network['Parents'] = network['Parents'].apply(lambda x: x.replace(', ', '","').replace('[', '["').replace(']', '"]'))
    network['Children'] = network['Children'].apply(eval)
    network['Parents'] = network['Parents'].apply(eval)
    network['Child1'] = network['Children'].apply(lambda x: x[0] if len(x) > 0 else None)
    network['Child2'] = network['Children'].apply(lambda x: x[1] if len(x) > 1 else None)
    network['Mother'] = network['Parents'].apply(lambda x: x[0] if len(x) > 0 else None)
    network['Father'] = network['Parents'].apply(lambda x: x[1] if len(x) > 1 else None)
    network['Partner'] = network['Spouse'].apply(lambda x: round(x) if pd.notnull(x) else None)
    network.rename(columns={'Education': 'Edu_level'}, inplace=True)
    levels = list(range(1,6))
    labels = ['Primary', 'Lower Secondary', 'Upper Secondary', 'Lower Tertiary', 'Upper Tertiary']
    network['Education'] = network['Edu_level'].replace(levels, labels)
    network['AgentID'] = network['AgentID'].astype(str)

    n1 = network[(pd.isnull(network.Partner) == False ) | (pd.isnull(network.Mother) == False) | (pd.isnull(network.Child1) == False)]
    n2 = network[(pd.isnull(network.Partner) == True ) & (pd.isnull(network.Mother) == True) & (pd.isnull(network.Child1) == True)]
    n1 = n1.groupby('AgentID', as_index=False).last() #Change to last or first depending on the use
    n2 = n2.groupby('AgentID', as_index=False).last()
    n1['Partner'] = n1['Spouse'].apply(lambda x: round(x) if pd.notnull(x) else None)
    df = pd.concat([n1, n2], ignore_index=True)
    df.drop(['Parents', 'Children', 'Spouse'], inplace=True, axis=1)
    df = df.drop(df[(df['AgentID'].duplicated() == True) & (df['Step'] == 0)].index)
    return df





def process_data(csv_path):
    network = pd.read_csv(csv_path, na_filter=True)
    network['Children'] = network['Children'].apply(lambda x: x.replace(', ', '","').replace('[', '["').replace(']', '"]'))
    network['Parents'] = network['Parents'].apply(lambda x: x.replace(', ', '","').replace('[', '["').replace(']', '"]'))
    network['Children'] = network['Children'].apply(eval)
    network['Parents'] = network['Parents'].apply(eval)
    network['Child1'] = network['Children'].apply(lambda x: x[0] if len(x) > 0 else None)
    network['Child2'] = network['Children'].apply(lambda x: x[1] if len(x) > 1 else None)
    network['Mother'] = network['Parents'].apply(lambda x: x[0] if len(x) > 0 else None)
    network['Father'] = network['Parents'].apply(lambda x: x[1] if len(x) > 1 else None)
    network['Partner'] = network['Spouse'].apply(lambda x: round(x) if pd.notnull(x) else None)
    network.rename(columns={'Education': 'Edu_level'}, inplace=True)
    levels = list(range(1,6))
    labels = ['Primary', 'Lower Secondary', 'Upper Secondary', 'Lower Tertiary', 'Upper Tertiary']
    network['Education'] = network['Edu_level'].replace(levels, labels)
    network['AgentID'] = network['AgentID'].astype(str)

    n1 = network[(pd.isnull(network.Partner) == False ) | (pd.isnull(network.Mother) == False) | (pd.isnull(network.Child1) == False)]
    n2 = network[(pd.isnull(network.Partner) == True ) & (pd.isnull(network.Mother) == True) & (pd.isnull(network.Child1) == True)]
    n1 = n1.groupby('AgentID', as_index=False).last() #Change to last or first depending on the use
    n2 = n2.groupby('AgentID', as_index=False).last()
    n1['Partner'] = n1['Spouse'].apply(lambda x: round(x) if pd.notnull(x) else None)
    df = pd.concat([n1, n2], ignore_index=True)
    df.drop(['Parents', 'Children', 'Spouse'], inplace=True, axis=1)
    df = df.drop(df[(df['AgentID'].duplicated() == True) & (df['Step'] == 0)].index)
    
    
    df['Capital'] = df['Capital'].apply(lambda x: x.replace(', ', '","').replace('[', '["').replace(']', '"]'))
    df['Capital'] = df['Capital'].apply(eval)
    df['Cultural'] = df['Capital'].apply(lambda x: x[0])
    df['Economic'] = df['Capital'].apply(lambda x: x[1])
    df['Social'] = df['Capital'].apply(lambda x: x[2])

    df['Child1_Edu'] = ""
    df['Child2_Edu'] = ""
    df['Child1_Income'] = ""
    df['Child2_Income'] = ""
    df['Mother_Edu'] = ""
    df['Father_Edu'] = ""
    df['Mother_Income'] = ""
    df['Father_Income'] = ""

    for i, row in df.iterrows():
        for j in range(1, 3):
            child_column = f"Child{j}"
            new_edu = f'Child{j}_Edu'
            new_income = f'Child{j}_Income'
            if not pd.isna(row[child_column]):
                child_id = str(row[child_column])
                df.at[i, new_edu] = df[df['AgentID'] == child_id].Edu_level.iloc[0] if not df[df['AgentID'] == child_id].empty else None
                df.at[i, new_income] = df[df['AgentID'] == child_id].Income.iloc[0] if not df[df['AgentID'] == child_id].empty else None
        
        spouse_id = str(int(row['Partner'])) if pd.isna(row['Partner']) == False else None
        df.at[i, 'Spouse_Edu'] = df[df['AgentID'] == spouse_id].Edu_level.iloc[0] if not df[df['AgentID'] == spouse_id].empty else None
        
        mother_id = str(row['Mother'])
        father_id = str(row['Father'])
        df.at[i, 'Mother_Edu'] = df[df['AgentID'] == mother_id].Edu_level.iloc[0] if not df[df['AgentID'] == mother_id].empty else None
        df.at[i, 'Father_Edu'] = df[df['AgentID'] == father_id].Edu_level.iloc[0] if not df[df['AgentID'] == father_id].empty else None
        df.at[i, 'Mother_Income'] = df[df['AgentID'] == mother_id].Income.iloc[0] if not df[df['AgentID'] == mother_id].empty else None
        df.at[i, 'Father_Income'] = df[df['AgentID'] == father_id].Income.iloc[0] if not df[df['AgentID'] == father_id].empty else None
        df.at[i, 'Mother_Cultural'] = df[df['AgentID'] == mother_id].Cultural.iloc[0] if not df[df['AgentID'] == mother_id].empty else None
        df.at[i, 'Mother_Economic'] = df[df['AgentID'] == mother_id].Economic.iloc[0] if not df[df['AgentID'] == mother_id].empty else None
        df.at[i, 'Mother_Social'] = df[df['AgentID'] == mother_id].Social.iloc[0] if not df[df['AgentID'] == mother_id].empty else None   
        df.at[i, 'Father_Cultural'] = df[df['AgentID'] == father_id].Cultural.iloc[0] if not df[df['AgentID'] == father_id].empty else None
        df.at[i, 'Father_Economic'] = df[df['AgentID'] == father_id].Economic.iloc[0] if not df[df['AgentID'] == father_id].empty else None
        df.at[i, 'Father_Social'] = df[df['AgentID'] == father_id].Social.iloc[0] if not df[df['AgentID'] == father_id].empty else None
        
    return df



def process_data_child3(csv_path):
    network = pd.read_csv(csv_path, na_filter=True)
    network['Children'] = network['Children'].apply(lambda x: x.replace(', ', '","').replace('[', '["').replace(']', '"]'))
    network['Parents'] = network['Parents'].apply(lambda x: x.replace(', ', '","').replace('[', '["').replace(']', '"]'))
    network['Children'] = network['Children'].apply(eval)
    network['Parents'] = network['Parents'].apply(eval)
    network['Child1'] = network['Children'].apply(lambda x: x[0] if len(x) > 0 else None)
    network['Child2'] = network['Children'].apply(lambda x: x[1] if len(x) > 1 else None)
    network['Child3'] = network['Children'].apply(lambda x: x[2] if len(x) > 2 else None)
    network['Mother'] = network['Parents'].apply(lambda x: x[0] if len(x) > 0 else None)
    network['Father'] = network['Parents'].apply(lambda x: x[1] if len(x) > 1 else None)
    network['Partner'] = network['Spouse'].apply(lambda x: round(x) if pd.notnull(x) else None)
    network.rename(columns={'Education': 'Edu_level'}, inplace=True)
    levels = list(range(1,6))
    labels = ['Primary', 'Lower Secondary', 'Upper Secondary', 'Lower Tertiary', 'Upper Tertiary']
    network['Education'] = network['Edu_level'].replace(levels, labels)
    network['AgentID'] = network['AgentID'].astype(str)

    n1 = network[(pd.isnull(network.Partner) == False ) | (pd.isnull(network.Mother) == False) | (pd.isnull(network.Child1) == False)]
    n2 = network[(pd.isnull(network.Partner) == True ) & (pd.isnull(network.Mother) == True) & (pd.isnull(network.Child1) == True)]
    n1 = n1.groupby('AgentID', as_index=False).last() #Change to last or first depending on the use
    n2 = n2.groupby('AgentID', as_index=False).last()
    n1['Partner'] = n1['Spouse'].apply(lambda x: round(x) if pd.notnull(x) else None)
    df = pd.concat([n1, n2], ignore_index=True)
    df.drop(['Parents', 'Children', 'Spouse'], inplace=True, axis=1)
    df = df.drop(df[(df['AgentID'].duplicated() == True) & (df['Step'] == 0)].index)
    
    
    df['Capital'] = df['Capital'].apply(lambda x: x.replace(', ', '","').replace('[', '["').replace(']', '"]'))
    df['Capital'] = df['Capital'].apply(eval)
    df['Cultural'] = df['Capital'].apply(lambda x: x[0])
    df['Economic'] = df['Capital'].apply(lambda x: x[1])
    df['Social'] = df['Capital'].apply(lambda x: x[2])

    df['Child1_Edu'] = ""
    df['Child2_Edu'] = ""
    df['Child3_Edu'] = ""
    df['Child1_Income'] = ""
    df['Child2_Income'] = ""
    df['Child3_Income'] = ""
    df['Mother_Edu'] = ""
    df['Father_Edu'] = ""
    df['Mother_Income'] = ""
    df['Father_Income'] = ""

    for i, row in df.iterrows():
        for j in range(1, 4):
            child_column = f"Child{j}"
            new_edu = f'Child{j}_Edu'
            new_income = f'Child{j}_Income'
            if not pd.isna(row[child_column]):
                child_id = str(row[child_column])
                df.at[i, new_edu] = df[df['AgentID'] == child_id].Edu_level.iloc[0] if not df[df['AgentID'] == child_id].empty else None
                df.at[i, new_income] = df[df['AgentID'] == child_id].Income.iloc[0] if not df[df['AgentID'] == child_id].empty else None
        
        spouse_id = str(int(row['Partner'])) if pd.isna(row['Partner']) == False else None
        df.at[i, 'Spouse_Edu'] = df[df['AgentID'] == spouse_id].Edu_level.iloc[0] if not df[df['AgentID'] == spouse_id].empty else None
        
        mother_id = str(row['Mother'])
        father_id = str(row['Father'])
        df.at[i, 'Mother_Edu'] = df[df['AgentID'] == mother_id].Edu_level.iloc[0] if not df[df['AgentID'] == mother_id].empty else None
        df.at[i, 'Father_Edu'] = df[df['AgentID'] == father_id].Edu_level.iloc[0] if not df[df['AgentID'] == father_id].empty else None
        df.at[i, 'Mother_Income'] = df[df['AgentID'] == mother_id].Income.iloc[0] if not df[df['AgentID'] == mother_id].empty else None
        df.at[i, 'Father_Income'] = df[df['AgentID'] == father_id].Income.iloc[0] if not df[df['AgentID'] == father_id].empty else None
        df.at[i, 'Mother_Cultural'] = df[df['AgentID'] == mother_id].Cultural.iloc[0] if not df[df['AgentID'] == mother_id].empty else None
        df.at[i, 'Mother_Economic'] = df[df['AgentID'] == mother_id].Economic.iloc[0] if not df[df['AgentID'] == mother_id].empty else None
        df.at[i, 'Mother_Social'] = df[df['AgentID'] == mother_id].Social.iloc[0] if not df[df['AgentID'] == mother_id].empty else None   
        df.at[i, 'Father_Cultural'] = df[df['AgentID'] == father_id].Cultural.iloc[0] if not df[df['AgentID'] == father_id].empty else None
        df.at[i, 'Father_Economic'] = df[df['AgentID'] == father_id].Economic.iloc[0] if not df[df['AgentID'] == father_id].empty else None
        df.at[i, 'Father_Social'] = df[df['AgentID'] == father_id].Social.iloc[0] if not df[df['AgentID'] == father_id].empty else None
        
    return df