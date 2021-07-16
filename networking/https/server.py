"""
Reference: https://realpython.com/python-https/
"""


from flask import Flask

import cryptography

app = Flask(__name__)


@app.route("/")
def get_secrete_message():
    return "Foo"


def main():
    app.run()


if __name__ == "__main__":
    main()
