import json
import os
import random


class DynamicJsonReader:

    @classmethod
    def INPUT_TYPES(s):
        return {
            "required": {
                "file_path": ("STRING", {
                    "default": "parameters.json"
                }),
            }
        }

    RETURN_TYPES = ("STRING", "INT", "FLOAT")
    RETURN_NAMES = ("string_value", "int_value", "float_value")
    FUNCTION = "read_json"
    CATEGORY = "custom_loaders"

    # Returning a random value here forces ComfyUI to execute this node on every run,
    # ensuring it grabs the freshly overwritten background file.
    @classmethod
    def IS_CHANGED(s, **kwargs):
        return random.random()

    def read_json(self, file_path):
        if not os.path.exists(file_path):
            return ("", 0, 0.0)

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                value = json.load(f)

            str_out = str(json.dumps(value))
            try:
                int_out = int(value)
            except:
                int_out = 0
            try:
                float_out = float(value)
            except:
                float_out = 0.0

            return (str_out, int_out, float_out)
        except Exception as e:
            print(f"Error reading JSON background file: {e}")
            return ("", 0, 0.0)


NODE_CLASS_MAPPINGS = {"DynamicJsonReader": DynamicJsonReader}
NODE_DISPLAY_NAME_MAPPINGS = {
    "DynamicJsonReader": "🔁 Dynamic JSON Field Reader"
}
