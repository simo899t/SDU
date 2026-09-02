package ROFLCLASSES;
public class InsertionSort {
    public static void main(String[] args) {
        int[] arr = {5, 2, 4, 6, 1, 3};
        insertionSort(arr);
        for (int i = 0; i < arr.length; i++) {
            System.out.print(arr[i] + " ");
        }
    }

    public static int[] insertionSort(int[] arr) {
        for (int i = 1; i < arr.length; i++) {
            // define key as the current element i.
            int key = arr[i];

            // define j as the element before i.
            int j = i - 1;
            while (j >= 0 && arr[j] > key) {
                // move the element to the right.
                arr[j+1] = arr[j];
                j = j - 1;
            }
            // insert the key at the right position.
            arr[j+1] = key;
        }
        return arr;
    }
}
