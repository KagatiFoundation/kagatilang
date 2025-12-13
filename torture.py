import subprocess
import sys
import time

if len(sys.argv) < 3:
    print("Usage: python torture.py <test_name> <iterations>")
    sys.exit(1)

test_name = sys.argv[1]
iterations = int(sys.argv[2])
stop_on_fail = False

print(f"Running test '{test_name}' {iterations} times...\n")
failures = []

for i in range(1, iterations + 1):
    cmd = ["cargo", "test", test_name, "--", "--nocapture", "--test-threads=1"]
    start = time.time()
    result = subprocess.run(cmd, capture_output=True, text=True)
    dur = (time.time() - start) * 1000
    if result.returncode != 0:
        print(f"{i}: failed ({dur:.2f} ms)")
        print(result.stdout)
        failures.append(i)
        if stop_on_fail:
            break
    else:
        print(f"{i}: passed ({dur:.2f} ms)")

print("\n--- SUMMARY ---")
print(f"Total: {iterations}")
print(f"Passed: {iterations - len(failures)}")
print(f"Failed: {len(failures)}")
if failures:
    print("Failed iterations:", failures)