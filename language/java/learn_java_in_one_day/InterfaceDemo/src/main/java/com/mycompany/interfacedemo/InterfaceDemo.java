/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 */
package com.mycompany.interfacedemo;

interface MyInterface {

    int myInt = 5;

    void someMethod();
}

class MyClass implements MyInterface {

    @Override
    public void someMethod() {
        System.out.println("This is a method implemented in MyClass");
    }
}

/**
 *
 * @author zuoxiang
 */
public class InterfaceDemo {

    public static void main(String[] args) {
        MyClass a = new MyClass();
        a.someMethod();
    }
}
