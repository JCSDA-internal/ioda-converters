#!/usr/bin/env python

import os
import requests
import argparse
from urllib.parse import urljoin
from datetime import datetime, timedelta

BASE_URL = "https://data.gats-inc.com/saber/Version2_0/SABER_cooling/NO_CoolingRate_Profiles"
DEFAULT_SAVE_DIR = "./saber_files"


def download_nc_files_by_date(date: datetime, save_dir: str):
    year = date.year
    jday = date.timetuple().tm_yday
    jday_str = f"{jday:03d}"
    url = f"{BASE_URL}/"

    print(f"Accessing: {url}")
    try:
        response = requests.get(url)
        response.raise_for_status()
    except requests.RequestException as e:
        print(f"Failed to access {url} - {e}")
        return

    os.makedirs(save_dir, exist_ok=True)

    fname = 'SABER_NO_PROFILE_FLUX_'+str(date.year)+jday_str+'_V1.0.nc'
    file_url = urljoin(url, fname)
    local_path = os.path.join(save_dir, fname)
    if not os.path.exists(local_path):
        print(f"Downloading {file_url}")
        try:
            with requests.get(file_url, stream=True) as r:
                r.raise_for_status()
                with open(local_path, 'wb') as f:
                    for chunk in r.iter_content(chunk_size=8192):
                        f.write(chunk)
        except requests.RequestException as e:
            print(f"Failed to download {fname}: {e}")
    else:
        print(f"Already exists: {fname}")

    print(f"Downloaded files for {date.year}.{jday_str} from {url}")


def main():
    parser = argparse.ArgumentParser(description="Download SABER NO Cooling Profile .nc files for a date range.")
    parser.add_argument("start_date", help="Start date in YYYY-MM-DD format")
    parser.add_argument("end_date", help="End date in YYYY-MM-DD format")
    parser.add_argument("-d", "--save-dir", dest="save_dir", default=DEFAULT_SAVE_DIR,
                        help=f"Directory to save the downloaded files (default: {DEFAULT_SAVE_DIR})")
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
        download_nc_files_by_date(current, args.save_dir)
        current += timedelta(days=1)


if __name__ == "__main__":
    main()
