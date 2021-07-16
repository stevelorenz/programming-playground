"""
Reference: https://realpython.com/python-https/
"""

import requests

DEFAULT_URL = "https://localhost:5000/"


def main():
    resp = requests.get(DEFAULT_URL, verify="ca-public-key.pem")
    msg = resp.text
    print(f"Secrete message: {msg}")


if __name__ == "__main__":
    main()
