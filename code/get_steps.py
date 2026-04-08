import subprocess
import yaml
from pathlib import Path
import sys
import argparse # To handle IDs passed from R
import ssl

import os

# --- FIX POUR L'ERREUR SSL ---
if (not os.environ.get('PYTHONHTTPSVERIFY', '') and
    getattr(ssl, '_create_unverified_context', None)):
    ssl._create_default_https_context = ssl._create_unverified_context
# -----------------------------

# ------------------------------------
# 1. LOAD CONFIGURATION
# ------------------------------------
script_dir = Path(__file__).resolve().parent 
project_root = script_dir.parent
config_path = project_root / "config.yml"

if not config_path.exists():
    print(f"ERROR: Configuration file not found at {config_path}")
    sys.exit(1)

with open(config_path, "r", encoding="utf-8") as f:
    cfg = yaml.safe_load(f)

# ------------------------------------
# 2. SETTINGS
# ------------------------------------
BIN_ROOT = Path(cfg['paths']['raw_bin'])
OUTPUT_ROOT = project_root / cfg['paths']['summaries']
OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
STEPCOUNT_EXE = f'"{sys.executable}" -m stepcount.stepcount'

# ------------------------------------
# 3. SELECTIVE LOGIC (New)
# ------------------------------------
# Check if R passed specific IDs as arguments
parser = argparse.ArgumentParser()
parser.add_argument('--ids', type=str, help='Comma separated list of IDs')
args = parser.parse_args()

target_ids = []
if args.ids:
    target_ids = [i.strip() for i in args.ids.split(',') if i.strip()]

# ------------------------------------
# FUNCTIONS
# ------------------------------------

def find_bin_files(root: Path, filter_ids=None):
    """Find files. If filter_ids is provided, only keep matching filenames."""
    all_files = sorted(root.rglob("*.bin"))
    if not filter_ids:
        return all_files
    
    # Only keep files where the filename stem matches one of the target_ids
    return [f for f in all_files if any(id_match in f.stem for id_match in filter_ids)]

def has_already_been_processed(bin_file: Path, output_root: Path) -> bool:
    file_stem = bin_file.stem
    per_file_output_dir = output_root / file_stem
    
    if not per_file_output_dir.is_dir():
        return False

    # On vérifie si un fichier CSV (le résultat final) existe dans le dossier
    # Stepcount génère souvent 'stepcount.csv' ou un fichier incluant le nom
    results = list(per_file_output_dir.glob("*.csv"))
    
    if len(results) > 0:
        # Optionnel : vérifier que le fichier n'est pas vide (0 octets)
        return results[0].stat().st_size > 0
        
    return False
    try:
        any_item = next(per_file_output_dir.iterdir(), None)
    except StopIteration:
        any_item = None
    return any_item is not None

def run_stepcount_on_file(bin_file: Path, output_root: Path):
    file_stem = bin_file.stem
    per_file_output_dir = output_root / file_stem
    per_file_output_dir.mkdir(parents=True, exist_ok=True)

    print(f"--- Processing: {bin_file.name} ---")
    
    cmd = [
        sys.executable, "-m", "stepcount.stepcount", 
        str(bin_file), 
        "-o", str(per_file_output_dir)
    ]
    
    result = subprocess.run(
        cmd, 
        capture_output=True, 
        text=True, 
        encoding="utf-8", 
        errors="ignore"
    )

    if result.returncode != 0:
        print(f"RESULT: FAILED for {bin_file.name}")
        print(f"ERROR: {result.stderr}") 
        return False
    
    print(f"RESULT: SUCCESS for {bin_file.name}")
    return True

# ------------------------------------
# MAIN
# ------------------------------------

def main():
    if not BIN_ROOT.is_dir():
        print(f"FATAL ERROR: BIN_ROOT invalid: {BIN_ROOT}")
        return

    bin_files = find_bin_files(BIN_ROOT, filter_ids=target_ids)
    
    processed_count = 0
    skipped_count = 0
    failed_count = 0
    total_files = len(bin_files)

    print(f"PYTHON START: Target files to check: {total_files}")
    if target_ids:
        print(f"Filtering for IDs: {target_ids}")

    for bin_file in bin_files:
        # If user selected specific IDs, we RE-PROCESS (overwrite) instead of skipping XX
        #if not target_ids and has_already_been_processed(bin_file, OUTPUT_ROOT):
        if has_already_been_processed(bin_file, OUTPUT_ROOT):
            skipped_count += 1
            continue

        success = run_stepcount_on_file(bin_file, OUTPUT_ROOT)
        if success:
            processed_count += 1
        else:
            failed_count += 1

    print("\n" + "="*30)
    print("STEPCOUNT EXECUTION SUMMARY")
    print(f"Total Files Handled: {total_files}")
    print(f"Successfully Done:   {processed_count}")
    print(f"Skipped (Existing):  {skipped_count}")
    print(f"Failed:              {failed_count}")
    print("="*30 + "\n")

if __name__ == "__main__":
    main()
