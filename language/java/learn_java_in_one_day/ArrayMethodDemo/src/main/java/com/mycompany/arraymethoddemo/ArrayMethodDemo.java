/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 */
package com.mycompany.arraymethoddemo;

import java.util.Arrays;

class MyClass {

    public void printFirstElement(int[] a) {
        System.out.println("The first element is " + a[0]);
    }

    public void passPrimitive(int primitivePara) {
        primitivePara = 10;
        System.out.println("Value inside method = " + primitivePara);
    }

    public void passReference(int[] refPara) {
        refPara[1] = 5;
        System.out.println("Value inside method: " + refPara[1]);
    }
}

/**
 *
 * @author zuoxiang
 */
public class ArrayMethodDemo {

    public static void main(String[] args) {
        MyClass amd = new MyClass();
        int[] myArray = {1, 2, 3, 4, 5};
        amd.printFirstElement(myArray);

        System.out.println(Arrays.toString(myArray));

        System.out.print("\n");

        int number = 2;
        System.out.println("number before = " + number);
        amd.passPrimitive(number);
        System.out.println("number after = " + number);

        System.out.println("myArray[1] before = " + myArray[1]);
        amd.passReference(myArray);
        System.out.println("myArray[1] after = " + myArray[1]);
    }
}
