#!/usr/bin/env python

output_file = "/tmp/perftest.cpp"

with open(output_file, "w") as f:
    f.write("#include <iostream>\n\n")
    f.write("int main() {\n")
    
    # Generate 25000 lambda variables
    for i in range(1, 250001):
        if i == 1:
            f.write(f"    auto lambda_{i} = []() {{ std::cout << \"{i}\" << std::endl; }};\n")
        else:
            f.write(f"    auto lambda_{i} = [&lambda_{i-1}]() {{ lambda_{i - 1}(); }};\n")
    
    # Call the last lambda
    f.write("    lambda_25000();\n")
    f.write("    return 0;\n")
    f.write("}\n")
