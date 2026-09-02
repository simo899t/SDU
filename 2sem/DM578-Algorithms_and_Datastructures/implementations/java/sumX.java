package ROFLCLASSES;
public class sumX {
    public static void main(String[] args) {
        int[] arr = {5, 2, 4, 6, 1, 3};
        int x = 11;
        System.out.println(findSumX(arr,x));
    }

    public static boolean findSumX(int[] arr, int x) {
        for (int i = 0; i < arr.length; i++) {
            // define key as the current element i.
            int key = arr[i];

            // check next elements in list.
            for (int j = i + 1; j < arr.length; j++) {
                // check if elements sum to x.
                if (key + arr[j] == x) {
                    return true;
                }    
            }
        }
        return false;
    }
}
