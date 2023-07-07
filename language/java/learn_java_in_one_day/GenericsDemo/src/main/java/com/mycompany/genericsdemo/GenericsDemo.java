/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 */
package com.mycompany.genericsdemo;

class MyGenericsClass<T extends Number> {

    private T myVar;
    
    public void setMyVar(T myVar) {
        this.myVar = myVar;
    }    
}

/**
 *
 * @author zuoxiang
 */
public class GenericsDemo {
    
    public static void main(String[] args) {
        System.out.println("Hello World!");
        
        MyGenericsClass g = new MyGenericsClass();
        g.setMyVar(6);
        g.setMyVar(6.1);
        
    }
}
