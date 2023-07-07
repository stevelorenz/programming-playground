/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.mycompany.objectorienteddemo;

/**
 *
 * @author zuoxiang
 */
public class Staff {

    private String nameOfStaff;
    // Final indicate that the variable CAN NOT be changed after it's created
    private final int hourlyRate = 30;
    private int hoursWorked;

    public Staff(String name) {
        this.nameOfStaff = name;
        System.out.println("\n" + this.nameOfStaff);
        System.out.println("---------------------------");
    }

    public Staff(String firstName, String lastName) {
        this.nameOfStaff = firstName + " " + lastName;
        System.out.println("\n" + this.nameOfStaff);
        System.out.println("---------------------------");
    }

    public String getNameOfStaff() {
        return nameOfStaff;
    }

    public int getHourlyRate() {
        return hourlyRate;
    }

    public int getHoursWorked() {
        return hoursWorked;
    }

    public void setHoursWorked(int hours) {
        if (hours > 0) {
            hoursWorked = hours;
        } else {
            System.err.println("Error: HoursWorked can not be negative");
        }
    }

    public void setNameOfStaff(String nameOfStaff) {
        this.nameOfStaff = nameOfStaff;
    }

    public void printMessage() {
        System.out.println("Calculating pay...");
    }

    public int calculatePay(int bonus, int allowance) {
        printMessage();
        if (hoursWorked > 0) {
            return hoursWorked * hourlyRate + bonus + allowance;
        } else {
            return 0;
        }
    }
}
