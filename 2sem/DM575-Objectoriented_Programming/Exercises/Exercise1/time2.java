package Exercise1;
import java.util.Scanner;
    class Durations {
        public static void main(String[] args) {
            Scanner sc = new Scanner(System.in);
            System.out.print("Please input seconds: "); 
            int Seconds = sc.nextInt();
            int years = Math.floorDiv(Seconds, (365*(24*(60*60)))), rest = Seconds - (years*(365*(24*(60*60)))) ;
            int days = Math.floorDiv(rest, (24*(60*60))), rest1 = Seconds - (days*(24*(60*60))) ;
            int hours = Math.floorDiv(rest1, (60*60)), rest2 = Seconds - (hours*(60*60)) ;
            int minutes = Math.floorDiv(rest2, 60), rest3 = rest2 - minutes*60;
            
            System.out.println("\n" + printTime(years, days,hours,minutes,rest3) + "\n");
        }

        private static String printTime(int years, int days, int hours, int minutes, int rest3) {
            if (years != 0) {
                return years + " years, " + days + " days, " + hours + " hours, " + minutes + " minutes and " + rest3 + " seconds";
            } else if (days != 0) {
                return days + " days, " + hours + " hours, " + minutes + " minutes and " + rest3 + " seconds";
            } else if (hours != 0) {
                return hours + " hours, " + minutes + " minutes and " + rest3 + " seconds";
            } else if (minutes != 0) {
                return minutes + " minutes and " + rest3 + " seconds";
            } else;
                return rest3 + " seconds";
        }
    }  


