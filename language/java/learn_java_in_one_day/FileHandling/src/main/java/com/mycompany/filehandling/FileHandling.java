/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 */
package com.mycompany.filehandling;

import java.io.*;

/**
 *
 * @author zuoxiang
 */
public class FileHandling {

    public static void main(String[] args) {
        System.out.println("Hello World!");

        String line;
        BufferedReader reader = null;

        try {
            reader = new BufferedReader(new FileReader("myFile.txt"));
            line = reader.readLine();
            while (line != null) {
                System.out.println(line);
                line = reader.readLine();
            }
        } catch (IOException e) {
            System.err.println(e.getMessage());
        } finally {
            try {
                if (reader != null) {
                    reader.close();
                }
            } catch (IOException e) {
                System.out.println(e.getMessage());
            }
        }
    }
}
