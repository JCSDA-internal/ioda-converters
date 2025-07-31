#!/usr/bin/env python 
import os
import re
import requests
import argparse
from urllib.parse import urljoin
from datetime import datetime, timedelta


BASE_URL = "https://data.gats-inc.com/saber/Version2_0/Level2A"
SAVE_DIR = "./saber_files"
            
def download_nc_files_by_date(date: datetime):
    year = date.year
    jday = date.timetuple().tm_yday
    jday_str = f"{jday:03d}"
    url = f"{BASE_URL}/{year}/{jday_str}/"
                
    print(f"Accessing: {url}")
    try:        
        response = requests.get(url)
        response.raise_for_status() 
    except requests.RequestException as e:
        print(f"Failed to access {url} - {e}") 
        return

    # Extract all .nc file links from HTML
    matches = re.findall(r'href="([^"]+\.nc)"', response.text)
    if not matches:
        print("No .nc files found.")
        return

    os.makedirs(SAVE_DIR, exist_ok=True)
    downloaded = 0

    for fname in matches:
        file_url = urljoin(url, fname)
        local_path = os.path.join(SAVE_DIR, fname)
        if not os.path.exists(local_path):
            print(f"Downloading {file_url}")
            try:
                with requests.get(file_url, stream=True) as r:
                    r.raise_for_status()
                    with open(local_path, 'wb') as f:
                        for chunk in r.iter_content(chunk_size=8192):
                            f.write(chunk)
                downloaded += 1
            except requests.RequestException as e:
                print(f"Failed to download {fname}: {e}")
        else:
            print(f"Already exists: {fname}")

    print(f"Downloaded {downloaded} files from {url}")


def main():
    parser = argparse.ArgumentParser(description="Download SABER Level2A .nc files for a date range.")
    parser.add_argument("start_date", help="Start date in YYYY-MM-DD format")
    parser.add_argument("end_date", help="End date in YYYY-MM-DD format")
    args = parser.parse_args()

    try:
        start = datetime.strptime(args.start_date, "%Y-%m-%d")
        end = datetime.strptime(args.end_date, "%Y-%m-%d")
    except ValueError as e:
        print(f"Invalid date format: {e}")
        return

    if start > end:
        print("Start date must not be after end date.")
        return

    current = start
    while current <= end:
        download_nc_files_by_date(current)
        current += timedelta(days=1)


if __name__ == "__main__":
    main()

