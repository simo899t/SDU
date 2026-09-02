package Exercise2;
import java.util.ArrayList;
class Arrays {
    public static void main(String[] args) {
        int[] v = {1, 2, 3, 4, 5};
        int[] w = {0, 1, 2, 3, 5};
        int n = 5;
        
        System.out.println("v = " + v);
        //System.out.println("The sum of alle elements in v is:" + sum(v));
        //System.out.println("There are: " + count(v) + " elements in v");
        //System.out.println(smallerThan(v,n) + " element in v, smaller than " + n );
        //System.out.println("The texual representation of v is: " + stringify(v));
        //squares(v);
        //reversed(v);
        //System.out.println("The decreasing squares from " + n + " are: " + decreasingSquares(n));
        //System.out.println("Divisors of " + n + " are: " + divisors(n));
        //System.out.println("Largest element of v is: " + max(v));
        //System.out.println("If v is a subset of w: " + subset(v,w));
        //System.out.println("Intersection of v and w: " + intersection(v,w));
        //System.out.println("Fist max position: " + firstPositionMax(v));
        //System.out.println("Last max position: " + lastPositionMax(v));
        //System.out.println("All max position: " + PositionsMax(v));
        //System.out.println("squares all elements: " + square(v));
        //System.out.println("Reverse v" + reverse(v));
        //System.out.println("Joins two arrays: " + join(v,w));
        System.out.println("Joins two arrays with sorted elements: " + stringify(sortedJoin(v,w)));
        //System.out.println("Joins two arrays with alernated elements: " + alernated(v,w));
        //System.out.println("Shuffles two lists " + shuffle(v,w));
        //System.out.println("If v is sorted" + isSorted(v));
    }

    /**
     * Sums all elements in v.
     * @param v
     */
    private static int sum(int[] v) {
        int sum = 0;
        for (int i = 0; i < v.length;) {
            sum += v[i];
        }
        return sum;
    }

     /**
     * Counts number of elements in w.
     * @param v
     */
    private static int count(int[] v) {
        return v.length;
    }

     /**
     * Counts elements in v smaller than n.
     * @param v
     * @param n
     */
    private static int smallerThan(int[] v, int n) {
        int count = 0;
        for (int i = 0; i < v.length; i++) {
            if (v[i] < n) {
                count++;
            }
        }
        return count;
    }

     /**
     * Returns textual representation of v.
     * @param v
     */
    private static String stringify(int[] v) {
        return v.toString();
    }

     /**
     * Squares all element in v as a new list.
     * @param v
     */
    private static void squares(int[] v) {
        for (int i = 0; i < v.length; i++) {
            System.out.println((int) Math.pow(v[i],2));
        }
    }

     /**
     * Returns a new array with elements reversed.
     * @param v
     */
    private static void reversed(int[] v) {
        for (int i = 0; i < v.length; i++) {
            System.out.println(v[(v.length-1)-i]);;
        }
    }

     /**
     * Returns an array with all perfect squares less than n.
     * @param v
     */
    private static int[] decreasingSquares(int n) {
        int[] result = {0};
        int index = 0;
        for (int i = 0; i < n; i++) {
            if ((int) Math.pow(i,2) < n) {
                result = growArray(result);
                result[index] = i;
                index++;
            }
        }
        return result;
    }

    /**
     * Grows an array-length with 1.
     * @param v
     * @return
     */
    private static int[] growArray(int[] v) {
        int size = 1;
        if (v != null) {
            size = v.length + 1;
        }
        return new int[size];
    }

     /**
     * Returns an array with all divisors of n.
     * @param v
     */
    private static int[] divisors(int n) {
        int[] result = {0};
        int index = 0;
            for (int i = 0; i < n; i++) {
                if (Math.floorDiv(n,i) == 0) {
                    result = growArray(result);
                    result[index] = i;
                    index++;
                }
            }
        return result;
    }

     /**
     * Returns the largest element in v.
     * @param v
     */
    private static int max(int[] v) {
        int result = 0;
        for (int i = 0; i < v.length; i++) {
            if (v[i] > result) {
                result = v[i];
            }
        }
        return result;
    }

     /**
     * Checks if array v is a subset of array w
     * @param v
     * @param w
     */
    private static boolean subset(int[] v, int[] w) {
        boolean result = false;
        for (int i = 0; i < v.length; i++) {
            for (int j = 0; j < w.length;) {
                if (i == j) {
                    result = true;
                    break;
                }
            }
        if (!result){
            return false;
        }
        }
    return true;
    }

     /**
     * Returns the intersection of v and w
     * @param v
     */
    private static int[] intersection(int[] w, int[] v) {
        int[] temp = {Math.min(w.length,v.length)};
        int resultLength = 0;
        for (int i = 0; i < w.length; i++) {
            for (int j = 0; j < v.length; j++) {
                if (w[i] == v[j]) {
                    temp[resultLength] = w[i];
                    resultLength++;
                }
            }
        }
        int[] result = new int[resultLength];
        for (int i = 0; i < resultLength; i++) {
            result[i] = temp[i];
        }
        return result;
    }

     /**
     * Returns the first position of the max element in v
     * @param v
     */
    private static int firstPositionMax(int[] v) {
        int pos = 0;
        for (int i = 1; i < v.length; i++) {
            if (v[i] > v[pos]) {
                pos = i;
            }
        }
        return pos;
    }

     /**
     * Sums all elements in v
     * @param v
     */
    private static int lastPositionMax(int[] v) {
        int pos = 0;
        for (int i = 1; i < v.length; i++) {
            if (v[i] >= v[pos]) {
                pos = i;
            }
        }
        return pos;
    }

     /**
     * Sums all elements in v
     * @param v
     */
    private static int[] PositionsMax(int[] v) {
        int[] temp = {v.length};
        int count = 0;
        int pos = 0;
        for (int i = 0; i < v.length; i++) {
            if (v[i] >= v[pos]) {
                temp[count] = i;
                count++;
                pos = 1;
            }
        }
        int[] result = {count};
        for (int i = 0; i < count; i++) {
            result[i] = temp[i];
            }
        return result;
    }

     /**
     * Squares all elements in v
     * @param v
     */
    private static int[] square(int[] v) {
        for (int i = 0; i < v.length; i++) {
            v[i] = (int) Math.pow(v[i],2);
        }
        return v;
    }

     /**
     * Reverses v
     * @param v
     */
    private static int[] reverse(int[] v) {
        for (int i = 0; i > (v.length-1)/2; i++)
            v[i] = v[v.length-i];
        return v;
    }

     /**
     * Joins w and v
     * @param v
     */
    private static int[] join(int[] v, int[] w) {
        int[] result = new int[w.length + v.length];

        // Copy w into result from index 0 to w.length
        System.arraycopy(w, 0, result, 0, w.length);

        // Copy v into result from index w.length to v.length
        System.arraycopy(v, 0, result, w.length, v.length);
        return result;
    }

     /**
     * Joins w and v with sorted elements
     * @param v
     * @param w
     */
    private static int[] sortedJoin(int[] v, int[] w) {
        int[] result = {v.length + w.length};
        int i = 0;
        int j = 0;
        int k = 0;
        for (; k < result.length; k++) {
            if (v[i] < w[j]) {
                result[k] = v[i];
                i++;
            } else {
                result[k] = w[j];
                j++;
            }
        }
        int newIndex = (i > j) ? i : j + 1;
        if (v.length < w.length) {
            for (; newIndex < result.length; newIndex++) {
                result[k] = w[newIndex];
            }
        } else {
            for (; newIndex < result.length; newIndex++) {
                result[k] = v[newIndex];
            }
        }
        return result;
        }

     /**
     * Joins w and v with alernated elements
     * @param v
     * @param w
     */
    private static int[] alernated(int[] v, int[] w) {
        int[] result = join(v, w);

        return result;
    }

     /**
     * Joins w and v with shuffled elements
     * @param v
     * @param w
     */
    private static int[] shuffle(int[] v, int[] w) {
        int[] result = {v.length + w.length};
        int i = 0;
        int j = 1;
        for (int k = 0; k < result.length; k+=2) {
            result[k] = v[i];
            i++;
            result[k+1] = w[j];
            j++;
            }
        return result;
    }

     /**
     * Checks if v is sorted
     * @param v
     */
    private static boolean isSorted(int[] v) {
        for (int i = 0; i < v.length; i++) {
            if (v[i] > v[i+1]) {
                return false;
            }
        }
        return true;
    }
}
