package ROFLCLASSES;

public class BinSearch {
    public static void main(String[] args) {
        int[] arr = {0};
        int x = 4;
        System.out.println(binSearch(arr, x));
    }

    public static int binSearch(int[] arr, int x) {
        int low = 0;
        int high = arr.length - 1;
        while (low != high) {
            int mid = (int) Math.ceil((low + high) / 2);
            if (arr[mid] == x) {
                return mid;
            } else if (arr[mid] < x) {
                low = mid;
            } else {
                high = mid;
            }
        }
        return -1;
    }
}


