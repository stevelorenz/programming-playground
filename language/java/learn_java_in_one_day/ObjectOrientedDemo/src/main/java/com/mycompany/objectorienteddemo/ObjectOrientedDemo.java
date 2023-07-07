/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 */
package com.mycompany.objectorienteddemo;

/**
 *
 * @author zuoxiang
 */
public class ObjectOrientedDemo {

    public static void main(String[] args) {
        Staff staff1 = new Staff("Peter");
        staff1.setHoursWorked(160);
        int pay = staff1.calculatePay(1000, 400);
        System.out.println("Pay= " + pay);
    }
}
