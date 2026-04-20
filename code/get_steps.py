import subprocess
import yaml
from pathlib import Path
import sys
import argparse 
import ssl
import os

# --- FIX POUR L'ERREUR SSL ---
if (not os.environ.get('PYTHONHTTPSVERIFY', '') and
    getattr(ssl, '_create_unverified_context', None)):
    ssl._create_default_https_context = ssl._create_unverified_context
# -----------------------------

# ------------------------------------
# 1. DYNAMIQUE : CHEMINS ET JAVA PORTABLE
# ------------------------------------
script_dir = Path(__file__).resolve().parent 
project_root = script_dir.parent

# --- INJECTION JAVA PORTABLE ---
# On cible resources/java_env/bin
java_bin_path = project_root / "resources" / "java_env" / "bin"

if java_bin_path.exists():
    # On ajoute le chemin de Java au DEBUT du PATH pour cette session
    os.environ["PATH"] = str(java_bin_path) + os.pathsep + os.environ["PATH"]
    # On définit aussi JAVA_HOME pour la compatibilité maximale
    os.environ["JAVA_HOME"] = str(java_bin_path.parent)
    print(f"--- SUCCESS: Portable Java Active: {java_bin_path}")
else:
    print("--- WARNING: Portable Java not found in resources/java_env/bin")
    print("--- Using system default Java (this might fail if Java is not installed)")

# ------------------------------------
# 2. LOAD CONFIGURATION
# ------------------------------------
config_path = project_root / "config.yml"

if not config_path.exists():
    print(f"ERROR: Configuration file not found at {config_path}")
    sys.exit(1)

with open(config_path, "r", encoding="utf-8") as f:
    cfg = yaml.safe_load(f)

# ------------------------------------
# 3. SETTINGS & ARGUMENTS
# ------------------------------------
parser = argparse.ArgumentParser()
parser.add_argument('--ids', type=str, help='Comma separated list of IDs')
parser.add_argument('--batch', type=str, help='Batch name for folder organization')
args = parser.parse_args()

target_ids = []
if args.ids:
    target_ids = [i.strip() for i in args.ids.split(',') if i.strip()]

# Résolution des chemins depuis le config.yml
BIN_ROOT = Path(cfg['paths']['raw_bin'])

if args.batch:
    OUTPUT_ROOT = project_root / cfg['paths']['summaries'] / args.batch
else:
    OUTPUT_ROOT = project_root / cfg['paths']['summaries']

OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)

# ------------------------------------
# FUNCTIONS
# ------------------------------------

def find_bin_files(root: Path, filter_ids=None):
    """Trouver les fichiers .bin avec filtrage optionnel par ID."""
    all_files = sorted(root.rglob("*.bin"))
    if not filter_ids:
        return all_files
    return [f for f in all_files if any(f.name.startswith(id_match) for id_match in filter_ids)]

def has_already_been_processed(bin_file: Path, output_root: Path) -> bool:
    file_stem = bin_file.stem
    per_file_output_dir = output_root / file_stem
    done_flag = per_file_output_dir / "DONE.txt"
    return done_flag.exists()

def run_stepcount_on_file(bin_file: Path, output_root: Path):
    file_stem = bin_file.stem
    per_file_output_dir = output_root / file_stem
    per_file_output_dir.mkdir(parents=True, exist_ok=True)

    print(f"--- Processing: {bin_file.name} ---", flush=True)
    
    # Commande stepcount lancée via l'exécutable Python courant
    cmd = [
        sys.executable, "-m", "stepcount.stepcount", 
        str(bin_file), 
        "-o", str(per_file_output_dir)
    ]
    
    # subprocess.run héritera de l'os.environ["PATH"] modifié plus haut
    result = subprocess.run(
        cmd,
        stdout=sys.stdout,
        stderr=sys.stderr,
        text=True
    )
    
    if result.returncode != 0:
        print(f"RESULT: FAILED for {bin_file.name}", flush=True)
        return False

    print(f"RESULT: SUCCESS for {bin_file.name}", flush=True)
    done_flag = per_file_output_dir / "DONE.txt"
    done_flag.write_text("completed")
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

    print(f"PYTHON START: Batch [{args.batch if args.batch else 'None'}]")
    print(f"Target files to check: {total_files}")

    for bin_file in bin_files:
        print(f"Checking: {bin_file.name}", flush=True)

        if has_already_been_processed(bin_file, OUTPUT_ROOT):
            skipped_count += 1
            print(f"Skipping already processed: {bin_file.name}", flush=True)
            continue

        success = run_stepcount_on_file(bin_file, OUTPUT_ROOT)
        if success:
            processed_count += 1
        else:
            failed_count += 1

    print("\n" + "="*30)
    print("STEPCOUNT EXECUTION SUMMARY", flush=True)
    print(f"Batch:               {args.batch}")
    print(f"Total Files Handled: {total_files}")
    print(f"Successfully Done:   {processed_count}")
    print(f"Skipped (Existing):  {skipped_count}")
    print(f"Failed:              {failed_count}")
    print("="*30 + "\n")

if __name__ == "__main__":
    main()
