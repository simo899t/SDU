package Exercise4;

public class Date {

    private int year;
    private int month;
    private int day;
    private TimeStamp time;

    public static void main(String[] args) {
        TimeStamp time = new TimeStamp(1,59,59);
        Date date1 = new Date(2024,2,28,time);
        System.out.println(date1.DatetoString());
        Date date2 = date1.clone();
        date2.skipDay();
        System.out.println(date2.DatetoString());
    }

    public Date(int year, int month, int day) {
        this.year = year;
        this.month = month;
        this.day = day;
        this.time = new TimeStamp(0,0,0);
    }

    public Date(int year, int month, int day, TimeStamp time) {
        this.year = year;
        this.month = month;
        this.day = day;
        this.time = time;
    }

    public int getYear() {
        return this.year;
    }

    public int getMonth() {
        return this.month;
    }

    public int getDay() {
        return this.day;
    }
    
    public TimeStamp getTime() {
        return this.time;
    }

    public void setYear(int year) {
        this.year = year;
    }

    public void setMonth(int month) {
        this.month = month;
    }

    public void setDay(int day) {
        this.day = day;
    }

    public boolean valid(int year, int month, int day) {
        return year >= 0 && month >= 0 && day >= 0;
    }

    public void skipDay() {
        setDay(getDay() + 1);
        if ((getMonth() == 1 || getMonth() == 3 || getMonth() == 5 || 
            getMonth() == 7|| getMonth() == 8|| getMonth() == 10|| 
            getMonth() == 12) && (getDay() > 31)) {
                skipMonth();
            }
        else if ((getMonth() == 4 || getMonth() == 6 || 
            getMonth() == 9 || getMonth() == 11) && (getDay() > 30)) {
                skipMonth(); 
                }
        else if ((getYear()%4 == 0) && (getDay() > 29)) {
                skipMonth();
            }
        else {
            if (getDay() > 28) {
                skipMonth();
            }
        }
    }

    public void skipMonth() {
        setMonth(getMonth() + 1);
        if (getMonth() > 12) {
            skipYear();
        }
    }

    public void skipYear() {
        setYear(getYear() + 1);
        
    }

    public Date clone() {
        return new Date(getYear(), getMonth(), getDay(), getTime());
    }

    public String DatetoString() {
        return  getDay() + "/" +  getMonth() + "/" +  getYear() + ": " + getTime().TimeToString();
        
    }
}
