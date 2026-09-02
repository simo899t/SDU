package Exercise5;
import java.util.Arrays;
import java.util.Random;

public class SortingsTest {

    private static final SortingAlgorithm sa = Sortings.BUBBLE_SORT;

    public static void main(String[] args) {
        System.out.println("charArray is sorting: " + charArray());
        System.out.println("stringArray is sorting: " + stringArray());
        System.out.println("randomArray is sorting: " + randomArray());
        System.out.println("emptyArray is sorting: " + emptyArray());
        System.out.println("edgeArray is sorting: " + edgeArray());
        System.out.println("sortedArray is sorting: " + sortedArray());
    }

    public static boolean charArray() {
        Character[] arr = randomCharList(5);
        sa.sort(arr);
        return isSorting(arr);
    }

    public static boolean stringArray() {
        String[] arr = randomStringList(5);
        sa.sort(arr);
        return isSorting(arr);
    }

    public static boolean randomArray() {
        Integer[] arr = randomIntList(5);
        sa.sort(arr);
        return isSorting(arr);
    }

    public static boolean emptyArray() {
        Integer[] arr = new Integer[0];
        sa.sort(arr);
        return isSorting(arr);
    }

    public static boolean edgeArray() {
        Integer[] arr = randomIntList(2);
        sa.sort(arr);
        return isSorting(arr);
    }

    public static boolean sortedArray() {
        Integer[] arr = sortedIntlist(5);
        sa.sort(arr);
        return isSorting(arr);
    }

    public static <T> void printArray(T[] a) {
        for (int i = 0; i < a.length; i++) {
            System.out.print("(" + a[i] + ")");
        }
    }

    public static Integer[] randomIntList(int n) {
        Integer[] newList = new Integer[n];
        Random random = new Random();
        for (int i = 0; i < newList.length; i++) {
            newList[i] = random.nextInt(100);
        }
        return newList;
    }

    public static Integer[] sortedIntlist(int n) {
        Integer[] newList = new Integer[n];
        for (int i = 0; i < newList.length; i++) {
            newList[i] = i;
        }
        return newList;
    }

    public static Character[] randomCharList(int n) {
        Character[] newList = new Character[n];
        Random random = new Random();
        for (int i = 0; i < newList.length; i++) {
            newList[i] = (char) (random.nextInt(26) + 'a'); // Generate random lowercase letters
        }
        return newList;
    }

    public static String[] randomStringList(int n) {
        String[] newList = new String[n];
        Random random = new Random();
        for (int i = 0; i < newList.length; i++) {
            newList[i] = generateRandomString(random, 10); // Generate random strings of length 10
        }
        return newList;
    }

    private static String generateRandomString(Random random, int length) {
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            sb.append((char) (random.nextInt(26) + 'a')); // Generate random lowercase letters
        }
        return sb.toString();
    }

    public static <T extends Comparable<T>> boolean isSorting(T[] arr) {
        for (int i = 0; i < arr.length - 1; i++) {
            if (arr[i].compareTo(arr[i + 1]) > 0) {
                return false;
            }
        }
        return true;
    }
}
