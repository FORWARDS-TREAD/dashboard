import json
import os
from datetime import datetime, timedelta

def generate_data():
    base_path = "data/stations/sensors_data/cardena"
    source_files = [
        os.path.join(base_path, "2026-02", "2026-02-04.json"),
        os.path.join(base_path, "2026-02", "2026-02-05.json")
    ]

    # Load source data
    data_samples = []
    for f in source_files:
        with open(f, 'r') as file:
            data_samples.append(json.load(file))

    start_date = datetime(2026, 1, 1)
    end_date = datetime(2027, 12, 31)
    
    current_date = start_date
    while current_date <= end_date:
        # Pick a sample (alternate based on day)
        sample_idx = current_date.day % len(data_samples)
        sample = data_samples[sample_idx]
        
        # Create a deep copy of the sample data to modify
        # Actually, we only need to modify fecha_raw
        new_data = json.loads(json.dumps(sample))
        
        date_str = current_date.strftime("%Y-%m-%d")
        month_str = current_date.strftime("%Y-%m")
        
        for element in new_data['data']:
            for sensor in element['sensores']:
                for entry in sensor['datos']:
                    # entry['fecha_raw'] is like "2026-02-04 23:57:00"
                    time_part = entry['fecha_raw'].split(' ')[1]
                    entry['fecha_raw'] = f"{date_str} {time_part}"
        
        # Save to file
        output_dir = os.path.join(base_path, month_str)
        os.makedirs(output_dir, exist_ok=True)
        output_file = os.path.join(output_dir, f"{date_str}.json")
        
        with open(output_file, 'w') as f:
            json.dump(new_data, f)
            
        current_date += timedelta(days=1)

if __name__ == "__main__":
    generate_data()
