import os, sys
print(sys.version)
env = os.environ
for (key, value) in env.items():
    print(key, value)
