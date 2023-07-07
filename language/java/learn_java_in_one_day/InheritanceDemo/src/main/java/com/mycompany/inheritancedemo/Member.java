/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.mycompany.inheritancedemo;

import java.util.Scanner;

/**
 *
 * @author zuoxiang
 */
abstract public class Member {

    public String welcome = "Welcome to ABC Fitness";
    protected double annualFee;
    private String name;
    private int memberID;
    private int memberSince;
    private int discount;

    public Member(String name, int memberID, int memberSince) {
        this.name = name;
        this.memberID = memberID;
        this.memberSince = memberSince;
    }

    public int getDiscount() {
        return discount;
    }

    public void setDiscount() {
        Scanner input = new Scanner(System.in);
        String password;

        System.out.println("Please enter the admin password: ");
        password = input.nextLine();

        if (!password.equals("abcd")) {
            System.err.println("Invalid password");
        } else {
            System.out.println("Please enter the discount: ");
            this.discount = input.nextInt();
        }
    }

    public void displayMemInfo() {
        System.out.println("Member Name: " + name);
        System.out.println("Member ID: " + memberID);
        System.out.println("Member Since " + memberSince);
        System.out.println("Annual Fee: " + annualFee);
    }

    abstract public void calculateAnnualFee();
}
