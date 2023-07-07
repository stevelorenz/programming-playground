/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 */
package com.mycompany.errordemo2;

import java.util.Scanner;
import java.util.InputMismatchException;

/**
 *
 * @author zuoxiang
 */
public class ErrorDemo2 {

    public static void main(String[] args) {
        int choice = 0;
        Scanner input = new Scanner(System.in);

        int[] numbers = {10, 11, 12, 13, 14, 15};

        System.out.println("Please enter the index of the array: ");

        try {
            choice = input.nextInt();
            System.out.printf("numbers[%d] = %d\n", choice, numbers[choice]);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Error: index is invalid.");
        } catch (InputMismatchException e) {
            System.out.println("Error: You didn't input an integer!");
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }
}
