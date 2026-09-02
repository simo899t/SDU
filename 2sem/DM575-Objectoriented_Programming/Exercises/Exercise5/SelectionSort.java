package Exercise5;

public class SelectionSort implements SortingAlgorithm{
    
    @Override
    public <T extends Comparable<T>> void sort(T[] arr) {
        for (int i = 0; i < arr.length; i++) {
            int smallestIndex = i;
            for (int j = i + 1; j < arr.length;j++) {
                if (arr[j].compareTo(arr[smallestIndex]) < 0) {
                    smallestIndex = j;
                }
            }
            T temp = arr[smallestIndex];
            arr[smallestIndex] = arr[i];
            arr[i] = temp;
        }
    }
}
