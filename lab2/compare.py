def compare_files(file1_path, file2_path):
    with open(file1_path, 'r') as file1, open(file2_path, 'r') as file2:
        for line1, line2 in zip(file1, file2):
            if line1 != line2:
                return False
        # Check if both files have the same number of lines
        if len(list(file1)) != len(list(file2)):
            return False
    return True


if(compare_files('big-data-labs\lab2\output1.txt','big-data-labs\lab2\output.txt')):
    print("files are identical")