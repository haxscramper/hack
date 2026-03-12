import torch

# 1. Check if PyTorch is using the ROCm build
print(f"PyTorch version: {torch.__version__}")
print(f"ROCm/HIP version: {torch.version.hip}")

# 2. Check if the GPU is accessible
cuda_available = torch.cuda.is_available()
print(f"Is ROCm GPU available?: {cuda_available}")

# 3. Get the name of your AMD GPU
if cuda_available:
    print(f"GPU Name: {torch.cuda.get_device_name(0)}")
    print(f"Device properties: {torch.cuda.get_device_properties(0)}")
else:
    print("PyTorch cannot see the GPU. It will fall back to CPU.")
