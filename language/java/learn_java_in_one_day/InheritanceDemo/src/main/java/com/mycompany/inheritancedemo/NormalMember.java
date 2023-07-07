/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.mycompany.inheritancedemo;

/**
 *
 * @author zuoxiang
 */
public class NormalMember extends Member {

    public NormalMember(String name, int memberID, int memberSince) {
        super(name, memberID, memberSince);
    }

    public void calculateAnnualFee() {
        this.annualFee = (1 - 0.01 * this.getDiscount()) * (100 + 12 * 30);
    }
}
