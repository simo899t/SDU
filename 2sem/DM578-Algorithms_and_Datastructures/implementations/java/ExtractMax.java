package ROFLCLASSES;

import java.util.ArrayList;
import java.util.Collections;

public class ExtractMax {

    public static void main(String[] args) {
        ArrayList<Integer> arr = new ArrayList<>();
        int n = 100;
        for (int i = n; i >= 0; i--)
            arr.add(i);
        extractMax(arr, 2);
        System.err.println(arr);
    }

    public static void extractMax(ArrayList<Integer> arr, int d) {
        Collections.swap(arr, 0, arr.size() - 1);
        arr.remove(arr.size() - 1);
        int i = 0;
        while (i < arr.size()) {
            int max = Integer.MIN_VALUE;
            int maxIdx = i + 1;
            for (int j = i + 1; j < arr.size() && j < i + 1 + d; j++) {
                if (arr.get(j) > max) {
                    max = arr.get(j);
                    maxIdx = j;
                }
            }
            if (arr.get(i) < max) {
                Collections.swap(arr, i, maxIdx);
            }
            i = maxIdx;
        }
    }
}


