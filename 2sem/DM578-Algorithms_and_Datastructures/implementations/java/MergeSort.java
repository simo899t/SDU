package ROFLCLASSES;
import java.util.Random;
import java.io.*;
import java.lang.*;

public class MergeSort {
    public static void main(String[] args) {
        int n = 1000000;
        for (int i = 0; i < 3; i++) {
            int[] arr = arrGen(n);
            long startTime = System.currentTimeMillis();
            arr = mergeSort(arr);
            long endTime = System.currentTimeMillis();
            //for (int i : arr) {
            //    System.out.println(i);
            //}
            System.out.println("arr lenth: " + n);
            System.out.println("runtime: " + (endTime - startTime));
            System.out.println("divided by nlogn: " + (endTime - startTime)/(n*log2(n)));
        }
    }   

    public static int[] mergeSort(int[] arr) {
        if (arr.length < 2) {
            return arr;
        }

        int mid = arr.length / 2;
        int[] left = new int[mid]; // 0 to mid
        int[] right = new int[arr.length - mid]; // mid to arr.length

        for (int i = 0; i < mid; i++) { // make new array from 0 to mid
            left[i] = arr[i];
        }

        for (int i = 0; i < arr.length - mid; i++) { // make new array from mid to arr.length
            right[i] = arr[i + mid];
        }

        left = mergeSort(left); // recursively sort left
        right = mergeSort(right); // recursively sort right
        
        return merge(left, right);
    }

    public static int[] merge(int[] left, int[] right) {
        int[] result = new int[left.length + right.length];
        int i = 0, j = 0;
        
        while (i < left.length && j < right.length) {
            if (left[i] <= right[j]) {
                result[i+j] = left[i];
                i++;
            } 
            else {
                result[i+j] = right[j];
                j++;
            }
        }

        while (i < left.length) {
            result[i+j] = left[i];
            i++;
        }

        while (j < right.length) {
            result[i+j] = right[j];
            j++;
        }
        return result;
    }

    public static int[] arrGen(int n) {
        int[] result = new int[n];
        Random random = new Random();
            for (int i = 0; i < n; i++) {
                result[i] = random.nextInt(10);
            }
        return result;
    }

    public static int log2(int N) {
        return (int)(Math.log(N) / Math.log(2));
    }
}
