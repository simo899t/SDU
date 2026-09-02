package Exercise5;

class BubbleSort implements SortingAlgorithm {
    
    @Override
    public <T extends Comparable<T>> void sort(T[] a) {
        //implement bubble sort
        T temp;
        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < a.length - i - 1; j++) {
                if (a[j].compareTo(a[j+1]) > 0) {
                    temp = a[j];
                    a[j] = a[j+1];
                    a[j+1] = temp;
                }
            }
        }
    }
}
