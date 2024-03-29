#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8

"""
The Python implementation of the GRPC helloworld.Greeter client.
"""

import logging

import grpc
import helloworld_pb2
import helloworld_pb2_grpc


def run():
    with grpc.insecure_channel("localhost:50051") as channel:
        stub = helloworld_pb2_grpc.GreeterStub(channel)
        response = stub.SayHello(helloworld_pb2.HelloRequest(name="Dark Soul"))
        print(f"Greeter client received: {response.message}")

        response = stub.SayHelloAgain(helloworld_pb2.HelloRequest(name="Dark Soul"))
        print(f"Greeter client received: {response.message}")


if __name__ == "__main__":
    logging.basicConfig()
    run()
