package Exercise5;

public interface SortingAlgorithm {
    public <T extends Comparable<T>> void sort(T[] a);
}
