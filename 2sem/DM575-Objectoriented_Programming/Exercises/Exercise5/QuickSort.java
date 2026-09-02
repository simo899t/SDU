package Exercise5;

public class QuickSort implements SortingAlgorithm{
    
    @Override
    public <T extends Comparable<T>> void sort(T[] arr) {
        sortRecursive(arr, 0, arr.length-1);
    }

    public <T extends Comparable<T>> void sortRecursive(T[] arr, int low, int high) {
        if (low < high) {
            int x = Partition(arr, low, high);
            sortRecursive(arr, low, x - 1);
            sortRecursive(arr, x + 1, high);
        }
    }

    public <T extends Comparable<T>> int Partition(T[] arr, int low, int high) {
        T pivot = arr[high];
        int i = low - 1;
        for (int j = low; j < high; j++) {
            if (arr[j].compareTo(pivot) <= 0) {
                i++;
                T temp = arr[i];
                arr[i] = arr[j];
                arr[j] = temp;
            }
        }
        T temp = arr[i+1];
        arr[i+1] = arr[high];
        arr[high] = temp;
        return i + 1;
    }
}
