import subprocess
from pathlib import Path

# Install "stepcount": https://cran.r-project.org/web/packages/stepcount/readme/README.html

# Each bin file takes about 15-30min to run

# ------------------------------------
# USER SETTINGS
# ------------------------------------

# Define Project Root
from pathlib import Path
PROJECT_ROOT = Path(__file__).resolve().parents[1]

# Root directory where your GENEActiv .bin files live (searched recursively)
# BIN_ROOT = Path("/Users/JWROBE8/Library/Mobile Documents/com~apple~CloudDocs/Documents/projects/2026/CARRS_Brain/data/BIN")
#BIN_ROOT = Path("C:/code/CARRS_Brain/data/BIN")
BIN_ROOT = PROJECT_ROOT / "data" / "BIN"

# Root directory where all stepcount outputs will be stored
# For each .bin, we create: OUTPUT_ROOT / <file_stem> /
# OUTPUT_ROOT = Path("/Users/JWROBE8/Library/Mobile Documents/com~apple~CloudDocs/Documents/projects/2026/CARRS_Brain/summaries")
# OUTPUT_ROOT = Path("C:/code/CARRS_Brain/summaries")
OUTPUT_ROOT = PROJECT_ROOT / "summaries"

# create OUTPUT_ROOT if not present
OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)

# Name/path of the stepcount executable
# (If `stepcount` works in your terminal, keep this as is.)
STEPCOUNT_EXE = "stepcount"

# ------------------------------------
# FUNCTIONS
# ------------------------------------

def find_bin_files(root: Path):
    """Recursively find all .bin files under BIN_ROOT."""
    return sorted(root.rglob("*.bin"))


def has_already_been_processed(bin_file: Path, output_root: Path) -> bool:
    """
    Determine whether this .bin file has already been processed.

    We define "processed" as:
    - There is a subdirectory under OUTPUT_ROOT named after the .bin file's stem,
    - And that directory contains at least one file.
    """
    file_stem = bin_file.stem
    per_file_output_dir = output_root / file_stem

    if not per_file_output_dir.is_dir():
        return False

    # Check if the directory has any contents
    try:
        any_item = next(per_file_output_dir.iterdir(), None)
    except StopIteration:
        any_item = None

    return any_item is not None


def run_stepcount_on_file(bin_file: Path, output_root: Path):
    """
    Run stepcount on a single .bin file and write outputs into
    OUTPUT_ROOT / <file_stem> /.
    """
    file_stem = bin_file.stem
    per_file_output_dir = output_root / file_stem
    per_file_output_dir.mkdir(parents=True, exist_ok=True)

    print(f"\n=== Processing {bin_file} ===")
    print(f"Output directory: {per_file_output_dir}")

    # Equivalent to:
    # stepcount "<bin_file>" -o "<per_file_output_dir>"
    cmd = [
        STEPCOUNT_EXE,
        str(bin_file),
        "-o",
        str(per_file_output_dir),
    ]

    print("Running command:")
    print(" ".join(cmd))
    print()

    #result = subprocess.run(cmd, capture_output=True, text=True)
    # Run stepcount as a subprocess and force UTF-8 decoding to avoid
    # Windows cp1252 UnicodeDecodeError when capturing stdout/stderr.
    # errors="ignore" prevents crashes caused by unexpected byte sequences.
    result = subprocess.run(cmd, capture_output=True, text=True, encoding="utf-8", errors="ignore")

    if result.returncode != 0:
        print("ERROR: stepcount failed for file:")
        print(f"  {bin_file}")
        print("\nSTDERR:")
        print(result.stderr)
        print("\nSTDOUT:")
        print(result.stdout)
        return

    print("stepcount completed successfully.")
    print("STDOUT:")
    print(result.stdout)
    print("STDERR (warnings, if any):")
    print(result.stderr)

    print("Files in output directory:")
    for p in sorted(per_file_output_dir.glob("*")):
        print(" -", p.name)


# ------------------------------------
# MAIN
# ------------------------------------

def main():
    if not BIN_ROOT.is_dir():
        raise NotADirectoryError(f"BIN_ROOT does not exist or is not a directory: {BIN_ROOT}")

    OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)

    bin_files = find_bin_files(BIN_ROOT)
    print(f"Found {len(bin_files)} .bin file(s) under {BIN_ROOT}")

    for bin_file in bin_files:
        if has_already_been_processed(bin_file, OUTPUT_ROOT):
            print(f"\n=== Skipping {bin_file} (already processed) ===")
            continue

        run_stepcount_on_file(bin_file, OUTPUT_ROOT)

    print("\nAll done.")


if __name__ == "__main__":
    main()
