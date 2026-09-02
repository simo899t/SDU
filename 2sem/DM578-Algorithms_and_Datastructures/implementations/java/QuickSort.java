package ROFLCLASSES;

import java.util.Random;
import java.util.Arrays;

public class QuickSort {
    
    public static void main(String[] args) {
        int n = 100000000;
        int time = 0;
        for (int i = 0; i < 3; i++) {
            int[] arr = arrGen(n);
            long startTime = System.currentTimeMillis();
            //quickSort(arr, 0, n-1);
            Arrays.sort(arr);
            long endTime = System.currentTimeMillis();
            //for (int i : arr) {
            //    System.out.println(i);
            //}
            time += (endTime - startTime);
        }
        System.out.println("arr lenth: " + n);
        System.out.println("AVG: " + time/3);
    }

    public static void quickSort(int[] arr, int low, int high) {
        if (low < high) {
            int x = Partition(arr,low,high);
            quickSort(arr,low, x - 1);
            quickSort(arr,x + 1, high);
        }
    }

    public static int Partition(int[] arr, int low, int high) {
        int x = arr[high];
        int i = low - 1;
        for (int j = low; j < high-1; j++) {
            if (arr[j] <= x) {
                i++;
                int temp = arr[i];
                arr[i] = arr[j];
                arr[j] = temp;
            }
        int temp = arr[i+1];
        arr[i+1] = arr[high];
        arr[high] = temp;
        }
        return i + 1;
    }

    public static int[] arrGen(int n) {
        int[] result = new int[n];
        Random random = new Random();
            for (int i = 0; i < n; i++) {
                result[i] = random.nextInt(10);
            }
        return result;
    }
}
