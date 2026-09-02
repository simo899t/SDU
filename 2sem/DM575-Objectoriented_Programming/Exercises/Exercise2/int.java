package Exercise2;
import java.util.Scanner;
class exercise {
    public static void main(String[] args) {
        try (Scanner sc = new Scanner(System.in)) {
            System.out.print("Please input an interger as n: "); 
            int n = sc.nextInt();
            System.out.print("Please input an interger as m: "); 
            int m = sc.nextInt();
            
            System.out.println("n: " + n + ", m: " + m);
            printMultiples(n, m);
            System.out.println("Sum up to " + n + " is: " + sumUpTo(n));
            System.out.println("Sum of all even numbers up to n is: " + sumEven(n));
            System.out.println("Factorial of " + n + " in: " + factorial(n));
            System.out.println("Double factorial of " + n + " is: " + doubleFactorial(n));
            System.out.println(n + " fibonacci number is: " + fibonacci(n));
            System.out.println("logarithm to n is approximately: " + log2(n));
            System.out.println("The n'th triangular number is: " + triangular(n));
            System.out.println("The n'th hexagonal number is: " + hexagonal(n));
            System.out.println("n has " + countDivisors(n) + " divisors");
            System.out.println("If n is a prime: " + isPrime(n));
            System.out.println("If n is perfect: " + isPerfect(n));
            System.out.println("There are " + countPrime(n) + " prime numbers below " + n);
            System.out.println("There are " + countPerfect(n) + " perfect numbers below " + n);
            System.out.println("The largest difference between two primes less than " + n + " is: " + largestDifference(n));
            System.out.println("lcm(" + n + ", " + m + ") is: " + lcm(n,m));
            System.out.println("hofstadterf of n is: " + hofstadterF(n));
            System.out.println("hofstadterm of n is: " + hofstadterM(n));
        }
    }

    /**
     * Prints all multiples of n smaller than m.
     * >> printMultiples(5, 25)
     * Multiples 0, 5, 10, 15, 20, 
     * @param n int
     * @param m int
     */
    public static void printMultiples(int n, int m) {
        System.out.print("Multiples on n is:");
        for (int i = 0; i < m; i++) {
            if (i % n == 0)
                System.out.print(i + ", ");
            }
        System.out.println("");
    }
    /**
     * Sums all numbers from 0 up to n.
     * >> sumUpTo(5)
     * Multiples 0, 5, 10, 15, 20, 
     * @param n int
     * @return int
     */
    public static int sumUpTo(int n) {
        if (n < 0) {
            return 0;
        } else {
            int result = 0;
            for (int i = 0; i <= n; i++) {
                result += i; 
                }
            return result;
            }
    }
    /**
     * Sums all even numbers up to n.
     * @param n int
     * @return int
     */
    public static int sumEven(int n) {
        if (n < 0) {
            return 0;
        } else {
            int result = 0;
            for (int i = 0; i <= n; i++) {
                if (i%2 == 0)
                result += i; 
                }
            return result;
            }
    }
    /**
     * Returns the factorial of n.
     * >> factorial(5)
     * 120
     * @param n int
     * @return int
     */
    public static int factorial(int n) {
        if (n < 0) {
            return 0;
        } else {
            int result = 1;
            for (int i = 1; i <= n; i++) {
                result = result * i; 
                }
            return result;
            }
    }
    /**
     * Returns the product of all even or uneven number up to n depending on n's parity.
     * >>doubleFactorial(5)
     * 10
     * @param n int
     * @return int
     */
    public static int doubleFactorial(int n) {
        try {
            if (n < 0)
            return 0;
        if (n%2 == 0) {
            int result = 1;
            for (int i = 1; i <= n; i++) {
                if (i%2 == 0)
                    result = result * i; 
            }
            return result;
        }
        int result = 1;
        for (int i = 1; i <= n; i++) {
            if (i%2 == 1)
                result = result * i; 
        }
        return result;
        } catch (Exception e) {
            return -1;
        }      
    }

    
    /**
     * Returns the n'th fibonacci number.
     * >>fibonacci(5)
     * 3
     * @param n
     * @return
     */
    public static int fibonacci(int n) {
            int first = 0;
            int second = 1;
            for (int i = 3; i <= n; i++) {
                int result = first + second;
                first = second;
                second = result;
                }
            return second;
            }
    
    /**
     * Returns the log2 to n, if log2(n) = 0, there is no answer under 500.
     * @param n int
     * @return int
     */
    public static int log2(int n) {
            return (int) (Math.log(n)/Math.log(2));
            }
    
    
    /**
     * Returns the n'th triangular number. This number where i starts as 1, 
     * and grows with one for each order. The result is the sum of all orders of i.
     * @param n int
     * @return int
     */
    public static int triangular(int n) {
        int result = 0;
        for (int i = 1; i < n+1; i++) {
            result += i;
        }
        return result;
    }
    /**
     * Returns the hex-number of n.
     * @param n
     * @return
     */
    public static int hexagonal(int n){
        if (n == 1) {
            return 1;
        } else if (n == 2) {
            return 6;
        } else {
            int result = 0;
            for (int i = 3; i < n; i++) {
                int start = 6;
                int add = 9;
                int update = add + 4;
                result = start + update;
            }
        return result;
        }
    }
    /**
     * Returns the number of divisors to n.
     * @param n
     * @return
     */
    public static int countDivisors(int n) {
        int count = 0;
        for (int i = 1; i < n; i++) {
            if (Math.floorDiv(n,i) == 0) {
                count += 1;
            }
        }
        return count;
    }
            /**
     * Checks if n is a prime.
     * @param n
     * @return
     */
    public static boolean isPrime(int n) {
        for (int i = 2; i < n-1; i++) {
            if (n%i == 0){
                return false;
                }
            }
        return true;
    }
    /**
     * Checks if number n is a perfect number.
     * @param n
     * @return
     */
    public static boolean isPerfect(int n) {
        int sum = 0;
        for(int i = 1; i < n; i++) {
           if(n % i == 0) {
              sum += i;
           }
        }
        return (sum == n);
    }
    /**
     * Returns the number of primes less than n.
     * @param n
     * @return
     */
    public static int countPrime(int n) {
        int count = 0;
        for (int i = 0; i <= n; i++) {
            if (isPrime(i)) {
                count += 1;
            }
        }
        return count;
    }
    /**
     * Returns the number of perfect numbers less than n.
     * @param n
     * @return
     */
    public static int countPerfect(int n) {
        int count = 0;
        for (int i = 0; i <= n; i++) {
            if (isPerfect(i)) {
                count += 1;
            }
        }
        return count;
    }
    /**
     * Returns the largest difference from two prime numbers less than n
     * @param n
     * @return
     */
    public static int largestDifference(int n) {
        if (n > 2) {
            for (int i = n; i > 2; i--){
                if (isPrime(i)) {
                    return i-2;
                }
            }
            return 0;
        }
        else {
            return 0;
        }
    }
    /**
     * Returns least common multiple of n & m.
     * @param n
     * @return
     */
    public static int lcm(int n, int m) {
        int largest = (n > m) ? n : m;
        while (true) {
            if (largest % n == 0 && largest % m == 0) {
                return largest;
            }
        largest++;
        }
    }
    /**
     * Returns the number of divisors to n
     * @param n
     * @return
     */
    public static int hofstadterF(int n) {
        if (n == 0) {
            return 1;
        } else {
            return n - hofstadterM(hofstadterF(n-1));
        }
    }
    /**
     * Returns the number of divisors to n
     * @param n
     * @return
     */
    public static int hofstadterM(int n) {
        if (n == 0) {
            return 0;
        } else {
            return n - hofstadterF(hofstadterM(n-1));
        }
    }
}