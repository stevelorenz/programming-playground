/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.mycompany.inheritancedemo;

/**
 *
 * @author zuoxiang
 */
public class VIPMember extends Member {

    public VIPMember(String name, int memberID, int memberSince) {
        super(name, memberID, memberSince);
    }

    public void calculateAnnualFee() {
        annualFee = (1 - 0.01 * getDiscount()) * 1200;
    }

}
