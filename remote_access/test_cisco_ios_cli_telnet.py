#!python3

import telnetlib
import unittest
import getpass
import time

# IMPORTANT: This server is only accessible inside the Cisco internal network !!!
SERVER_IP = "10.74.56.76"
SERVER_PORT = 3016
LOGIN_IS_REQUIRED = True
DEFAULT_USER = "admin"


class TestSDWANAsyncModule(unittest.TestCase):
    """Test SDWAN Async NIM support"""

    PASSWD: str = ""

    @classmethod
    def setUpClass(cls):
        print("* Connect to the router via telnet")
        cls.tn = telnetlib.Telnet(SERVER_IP, SERVER_PORT)
        if LOGIN_IS_REQUIRED:
            cls.tn.read_until(b"Username: ")
            cls.tn.write(DEFAULT_USER.encode("ascii") + b"\n")
            if not cls.PASSWD:
                cls.PASSWD = "Crdc123456!"
            cls.tn.read_until(b"Password: ")
            cls.tn.write(cls.PASSWD.encode("ascii") + b"\n")

    @classmethod
    def tearDownClass(cls):
        print("")
        print("* Logout the telnet connection")
        cls.tn.write(b"logout\n")
        cls.tn.close()

    def execute_one_cmd(self, cmd: str) -> str:
        self.tn.write(cmd.encode("ascii") + b"\n")
        time.sleep(1)
        ret = self.tn.read_very_eager().decode("ascii")
        return ret

    def test_dummy(self):
        self.assertEqual(1, 1)

    def test_sdn_mode(self):
        ret = self.execute_one_cmd("show version | inc operating")
        ret = ret.splitlines()
        print(ret)


def main():
    TestSDWANAsyncModule.PASSWD = getpass.getpass()

    print("*** Test ISR4K interfaces through telnet connection")
    unittest.main()


if __name__ == "__main__":
    main()
