import os
import csv
import requests
import time
import json
from dotenv import load_dotenv
import sys

# ------------------------------
# Load API key from .env
# ------------------------------
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.abspath(os.path.join(script_dir, ".."))
dotenv_path = os.path.join(project_root, ".env")
load_dotenv(dotenv_path)

API_KEY = os.getenv("GEMINI_API_KEY")
MODEL = "gemini-2.5-flash"

if not API_KEY:
    raise ValueError("GEMINI_API_KEY not found in .env")

# ------------------------------
# Function to classify addresses
# ------------------------------
def classify_address(address):
    url = f"https://generativelanguage.googleapis.com/v1beta/models/{MODEL}:generateContent?key={API_KEY}"
    headers = {"Content-Type": "application/json"}

    body = {
        "contents": [
            {
                "role": "user",
                "parts": [
                    {
                        "text": (
                            "Classify the following U.S. street address into one of these categories:\n"
                            "- Residential\n"
                            "- Commercial / Business / Office\n"
                            "- Mixed-use\n\n"
                            f"Address: {address}\n"
                            "Answer with only the category."
                        )
                    }
                ]
            }
        ]
    }

    resp = requests.post(url, json=body, headers=headers)
    resp.raise_for_status()
    result = resp.json()
    category = result["candidates"][0]["content"]["parts"][0]["text"].strip()
    return category

# ------------------------------
# Get input filename from command-line
# ------------------------------
if len(sys.argv) < 2:
    print("Usage: python address_classification.py <input_csv>")
    sys.exit(1)

input_file = sys.argv[1]
if not os.path.isfile(input_file):
    raise FileNotFoundError(f"Input file not found: {input_file}")

# ------------------------------
# Determine station column automatically
# ------------------------------
with open(input_file, newline="", encoding="utf-8") as infile:
    reader = csv.DictReader(infile)
    header = reader.fieldnames

if "start_station_name" in header:
    station_col = "start_station_name"
elif "end_station_name" in header:
    station_col = "end_station_name"
elif "station_name" in header:
    station_col = "station_name"
else:
    raise ValueError(
        "CSV must have one of the following columns: "
        "'start_station_name', 'end_station_name', or 'station_name'"
    )

# Output filename: append _gemini_classified before .csv
base, ext = os.path.splitext(input_file)
output_file = os.path.join(project_root, f"{base.split(os.sep)[-1]}_gemini_classified{ext}")

# Cache file (relative to project root)
cache_file = os.path.join(project_root, "address_cache.json")

# Load existing cache
if os.path.exists(cache_file):
    with open(cache_file, "r", encoding="utf-8") as f:
        cache = json.load(f)
else:
    cache = {}

# ------------------------------
# Read CSV and classify
# ------------------------------
with open(input_file, newline="", encoding="utf-8") as infile, \
     open(output_file, "w", newline="", encoding="utf-8") as outfile:

    reader = csv.DictReader(infile)
    writer = csv.writer(outfile)
    writer.writerow([station_col, "category"])

    for row in reader:
        station = row[station_col]

        if station.lower() == "not specified":
            category = "Unknown"
        elif station in cache:
            category = cache[station]
        else:
            category = classify_address(station)
            cache[station] = category
            time.sleep(3)  # longer sleep to reduce chance of rate-limiting

        writer.writerow([station, category])
        print(f"{station} → {category}")

# Save cache at the end
with open(cache_file, "w", encoding="utf-8") as f:
    json.dump(cache, f, indent=2)

print("Classification done! Saved to:", output_file)
