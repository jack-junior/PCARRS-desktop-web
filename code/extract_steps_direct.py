"""
Direct accelerometer step extraction - Alternative to stepcount
Processes GENEActiv .bin files to extract step counts without using stepcount library

This script:
1. Reads GENEActiv .bin files directly using actipy
2. Processes accelerometer data to detect steps
3. Generates summary CSV files with step counts per day/hour
"""

import os
import sys
from pathlib import Path
import warnings
warnings.filterwarnings('ignore')

# Check if actipy is installed, if not provide instructions
try:
    import actipy
    import pandas as pd
    import numpy as np
except ImportError as e:
    print(f"ERROR: Required package not found: {e}")
    print("\nPlease install required packages:")
    print("  python -m pip install actipy pandas numpy")
    sys.exit(1)


# ------------------------------------
# USER SETTINGS
# ------------------------------------

# Root directory where your GENEActiv .bin files live
BIN_ROOT = Path("C:/code/CARRS_Brain/data/BIN")

# Root directory where all outputs will be stored
OUTPUT_ROOT = Path("C:/code/CARRS_Brain/summaries")


# ------------------------------------
# FUNCTIONS
# ------------------------------------

def find_bin_files(root: Path):
    """Recursively find all .bin files under BIN_ROOT."""
    return sorted(root.rglob("*.bin"))


def has_already_been_processed(bin_file: Path, output_root: Path) -> bool:
    """
    Check if this .bin file has already been processed.
    """
    file_stem = bin_file.stem
    per_file_output_dir = output_root / file_stem
    
    if not per_file_output_dir.is_dir():
        return False
    
    # Check if summary CSV exists
    summary_file = per_file_output_dir / "Info.csv"
    return summary_file.exists()


def simple_step_detection(accel_magnitude, sample_rate=100):
    """
    Simple step detection algorithm using peak detection.
    
    Args:
        accel_magnitude: Vector magnitude of acceleration (numpy array)
        sample_rate: Sampling rate in Hz
    
    Returns:
        step_indices: Array of indices where steps were detected
    """
    from scipy.signal import find_peaks
    
    # Remove gravity (approximate 1g)
    accel_no_gravity = np.abs(accel_magnitude - 1.0)
    
    # Smooth the signal
    window = int(sample_rate * 0.1)  # 100ms window
    if window % 2 == 0:
        window += 1
    
    from scipy.signal import medfilt
    smoothed = medfilt(accel_no_gravity, kernel_size=window)
    
    # Find peaks (potential steps)
    # Typical walking frequency is 1-3 Hz, so minimum distance between steps
    min_distance = int(sample_rate * 0.3)  # 300ms between steps
    
    peaks, properties = find_peaks(
        smoothed,
        height=0.3,  # Minimum acceleration change for a step
        distance=min_distance
    )
    
    return peaks


def process_bin_file(bin_file: Path, output_dir: Path):
    """
    Process a single .bin file to extract step counts.
    
    Args:
        bin_file: Path to input .bin file
        output_dir: Directory for output files
    
    Returns:
        bool: True if successful, False otherwise
    """
    print(f"\n{'='*60}")
    print(f"Processing: {bin_file.name}")
    print(f"{'='*60}")
    print(f"Input file:  {bin_file}")
    print(f"Output dir:  {output_dir}")
    print(f"File size:   {bin_file.stat().st_size / (1024**2):.2f} MB")
    
    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)
    
    try:
        # Read the accelerometer data
        print("\nReading accelerometer data...")
        data, info = actipy.read_device(
            str(bin_file),
            lowpass_hz=20,
            calibrate_gravity=True,
            detect_nonwear=True,
            resample_hz=30  # Downsample to 30Hz for efficiency
        )
        
        print(f"✓ Data loaded: {len(data)} samples")
        print(f"  Sampling rate: {info.get('ResampleRate', 'Unknown')} Hz")
        print(f"  Duration: {(data.index[-1] - data.index[0]).total_seconds() / 3600:.1f} hours")
        
        # Calculate vector magnitude
        print("\nCalculating vector magnitude...")
        data['magnitude'] = np.sqrt(data['x']**2 + data['y']**2 + data['z']**2)
        
        # Detect steps
        print("Detecting steps...")
        sample_rate = info.get('ResampleRate', 30)
        step_indices = simple_step_detection(data['magnitude'].values, sample_rate=sample_rate)
        
        print(f"✓ Detected {len(step_indices)} steps")
        
        # Create step timestamps
        step_times = data.index[step_indices]
        step_df = pd.DataFrame({'time': step_times})
        
        # Aggregate by day
        step_df['date'] = step_df['time'].dt.date
        daily_steps = step_df.groupby('date').size().reset_index(name='steps')
        daily_steps.columns = ['date', 'steps']
        
        # Aggregate by hour
        step_df['datetime_hour'] = step_df['time'].dt.floor('H')
        hourly_steps = step_df.groupby('datetime_hour').size().reset_index(name='steps')
        hourly_steps.columns = ['datetime', 'steps']
        
        # Save outputs
        print("\nSaving outputs...")
        
        # Info file
        info_file = output_dir / "Info.csv"
        info_df = pd.DataFrame([{
            'file': bin_file.name,
            'total_steps': len(step_indices),
            'duration_hours': (data.index[-1] - data.index[0]).total_seconds() / 3600,
            'start_time': data.index[0],
            'end_time': data.index[-1],
            'sample_rate': sample_rate
        }])
        info_df.to_csv(info_file, index=False)
        print(f"  ✓ {info_file.name}")
        
        # Daily summary
        daily_file = output_dir / "daily_steps.csv"
        daily_steps.to_csv(daily_file, index=False)
        print(f"  ✓ {daily_file.name} ({len(daily_steps)} days)")
        
        # Hourly summary
        hourly_file = output_dir / "hourly_steps.csv"
        hourly_steps.to_csv(hourly_file, index=False)
        print(f"  ✓ {hourly_file.name} ({len(hourly_steps)} hours)")
        
        # Activity summary (similar to stepcount output)
        activity_file = output_dir / "activity_summary.csv"
        activity_df = pd.DataFrame({
            'date': daily_steps['date'],
            'steps': daily_steps['steps'],
            'step_rate_per_hour': daily_steps['steps'] / 24
        })
        activity_df.to_csv(activity_file, index=False)
        print(f"  ✓ {activity_file.name}")
        
        print(f"\n✓ Success! Generated 4 output files")
        return True
        
    except Exception as e:
        print(f"\n✗ Error processing file: {e}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """Main processing function."""
    print("=" * 60)
    print("DIRECT ACCELEROMETER STEP EXTRACTION")
    print("Alternative to stepcount using actipy + scipy")
    print("=" * 60)
    print()
    
    # Validate input directory
    if not BIN_ROOT.is_dir():
        print(f"✗ Input directory does not exist: {BIN_ROOT}")
        return
    
    # Create output directory
    OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
    
    print(f"Input directory:  {BIN_ROOT}")
    print(f"Output directory: {OUTPUT_ROOT}")
    
    # Find all .bin files
    bin_files = find_bin_files(BIN_ROOT)
    
    if not bin_files:
        print(f"\n✗ No .bin files found in {BIN_ROOT}")
        return
    
    print(f"\nFound {len(bin_files)} .bin file(s)")
    
    # Process each file
    success_count = 0
    skipped_count = 0
    failed_files = []
    
    for idx, bin_file in enumerate(bin_files, 1):
        print(f"\n[File {idx}/{len(bin_files)}]")
        
        # Check if already processed
        if has_already_been_processed(bin_file, OUTPUT_ROOT):
            print(f"⊘ Skipping {bin_file.name} (already processed)")
            skipped_count += 1
            continue
        
        # Create output directory for this file
        file_stem = bin_file.stem
        per_file_output_dir = OUTPUT_ROOT / file_stem
        
        # Process the file
        success = process_bin_file(bin_file, per_file_output_dir)
        
        if success:
            success_count += 1
        else:
            failed_files.append(bin_file.name)
    
    # Print summary
    print(f"\n{'='*60}")
    print("SUMMARY")
    print(f"{'='*60}")
    print(f"Total files:  {len(bin_files)}")
    print(f"Successful:   {success_count}")
    print(f"Skipped:      {skipped_count}")
    print(f"Failed:       {len(failed_files)}")
    
    if failed_files:
        print(f"\nFailed files:")
        for fname in failed_files:
            print(f"  - {fname}")
    
    print("\nAll done!")


if __name__ == "__main__":
    main()
