package Exercise1;
import java.util.Scanner;
class Time {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.println("Please input hours: "); 
        int Hours = sc.nextInt();
        System.out.println("Please input minutes: "); 
        int Minutes = sc.nextInt();
        System.out.println("Please input seconds: "); 
        int Seconds = sc.nextInt();
        int result = (Hours*60*60) + (Minutes*60) + Seconds;

        System.out.println("hours: " + Hours);
        System.out.println("minutes: " + Minutes);
        System.out.println("seconds: " + Seconds);
        System.out.println("Converted to seconds: " + result + "seconds.");
    }
}