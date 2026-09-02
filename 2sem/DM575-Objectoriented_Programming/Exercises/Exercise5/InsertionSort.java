package Exercise5;

public class InsertionSort implements SortingAlgorithm{
    
    @Override
    public <T extends Comparable<T>> void sort(T[] arr) {
        for (int i = 2; i < arr.length; i++) {
            T key = arr[i];
            int j = i - 1;
            while (j >= 0 && arr[j].compareTo(key) > 0) {
                arr[j + 1] = arr[j];
                j = j - 1;
            }
            arr[j + 1] = key;
        }
    }
}
