"""
About: The Python wrapper around the cstack library.
"""

from ctypes import *


class value_t(Structure):
    _fields_ = [("data", c_char_p), ("len", c_int)]


class _NativeStack(object):
    def __init__(self):
        self.stackLib = cdll.LoadLibrary("libcstack.so")

        self._makevalue_ = self.stackLib.make_value
        self._makevalue_.argtypes = [c_char_p, c_int]
        self._makevalue_.restype = value_t

        self._copyvalue_ = self.stackLib.copy_value
        self._copyvalue_.argtypes = [c_char_p, c_int]
        self._copyvalue_.restype = value_t

        self._freevalue_ = self.stackLib.free_value
        self._freevalue_.argtypes = [POINTER(value_t)]

        self._new_ = self.stackLib.cstack_new
        self._new_.argtypes = []
        self._new_.restype = c_void_p

        self._delete_ = self.stackLib.cstack_delete
        self._delete_.argtypes = [c_void_p]

        self._constructor_ = self.stackLib.cstack_constructor
        self._constructor_.argtypes = [c_void_p, c_int]

        self._destructor_ = self.stackLib.cstack_destructor
        self._destructor_.argtypes = [c_void_p, c_void_p]

        self._size_ = self.stackLib.cstack_size
        self._size_.argtypes = [c_void_p]
        self._size_.restype = c_int

        self._push_ = self.stackLib.cstack_push
        self._push_.argtypes = [c_void_p, value_t]
        self._push_.restype = c_int

        self._pop_ = self.stackLib.cstack_pop
        self._pop_.argtypes = [c_void_p, POINTER(value_t)]
        self._pop_.restype = c_int

        self._clear_ = self.stackLib.cstack_clear
        self._clear_.argtypes = [c_void_p, c_void_p]


class Stack(object):
    def __enter__(self):
        self._nativeApi_ = _NativeStack()
        self._handler_ = self._nativeApi_._new_()
        self._nativeApi_._constructor_(self._handler_, 100)
        return self

    def __exit__(self, type, value, traceback):
        self._nativeApi_._destructor_(self._handler_, self._nativeApi_._freevalue_)
        self._nativeApi_._delete_(self._handler_)

    def size(self):
        return self._nativeApi_._size_(self._handler_)

    def push(self, item):
        result = self._nativeApi_._push_(
            self._handler_,
            self._nativeApi_._copyvalue_(item.encode("utf-8"), len(item)),
        )
        if result != 1:
            raise Exception("Stack is full!")

    def pop(self):
        value = value_t()
        result = self._nativeApi_._pop_(self._handler_, byref(value))
        if result != 1:
            raise Exception("Stack is empty!")
        item = string_at(value.data, value.len)
        self._nativeApi_._freevalue_(value)
        return item

    def clear(self):
        self._nativeApi_._clear_(self._handler_, self._nativeApi_._freevalue_)


if __name__ == "__main__":
    with Stack() as stack:
        stack.push("TestA")
        stack.push("TestB")
        print("Size after pushes: {}".format(str(stack.size())))
        while stack.size() > 0:
            print(stack.pop())
        print("Size after pushes: {}".format(str(stack.size())))
        stack.push("TestC")
        stack.push("TestD")
        stack.clear()
        print("Size after clear: {}".format(str(stack.size())))
