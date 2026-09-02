package Exercise4;

public class TimeStamp {

    private int hours;
    private int minutes;
    private int seconds;
    
    public static void main(String[] args) {
        TimeStamp time = new TimeStamp(21,15,00);


        time.SkipTime(new TimeStamp(1, 0, 1));
        System.out.println(time.TimeToString());
    }

    /**
     * Default constructor for a time stamp of 0 hours.
     */
    public TimeStamp() {
        this.hours = 0;
        this.minutes = 0;
        this.seconds = 0;
    }

    /**
     * Constructor for a time stamp with a given number of seconds.
     * @param seconds
     */
    public TimeStamp(int seconds) {
        this.hours = seconds / 3600;
        this.minutes = (seconds % 3600) / 60;
        this.seconds = seconds - (hours * 3600) - (minutes * 60);
    }

    /**
     * Constructor for a time stamp with a given number of minutes.
     * @param seconds
     * @param minutes
     */
    public TimeStamp(int seconds, int minutes) {
        this.hours = seconds / 3600;
        this.minutes = (seconds % 3600) / 60;
        this.seconds = seconds % 60;
    }

    /**
     * Constructor for a time stamp with a given number of hours.
     * @param hours
     * @param minutes
     * @param seconds
     */
    public TimeStamp(int hours, int minutes, int seconds) {
        this.hours = hours + ((minutes + (seconds/60))/60);
        this.minutes = (minutes + (seconds/60))%60;
        this.seconds = seconds % 60;
    }

    /**
     * Get the number of hours in the time stamp.  
     * @return
     */
    public int getHours() {
        return this.hours;
    }

    /**
     * Get the number of minutes in the time stamp.
     * @return
     */
    public int getMinutes() {
        return this.minutes;
    }

    /**
     * Get the number of seconds in the time stamp.
     * @return
     */
    public int getSeconds() {
        return this.seconds;
    }

    /**
     * Set the number of hours in the time stamp.
     * @param hours
     */
    public void setHours(int hours) {
        this.hours = hours;
    }

    /**
     * Set the number of minutes in the time stamp.
     * @param minutes
     */
    public void setMinutes(int minutes) {
        this.minutes = minutes;
    }

    /**
     * Set the number of seconds in the time stamp.
     * @param seconds
     */
    public void setSeconds(int seconds) {
        this.seconds = seconds;
    }

    /**
     * Check if the time stamp is valid (0 or positive).
     * @return
     */
    public boolean valid() {
        return getHours() >= 0 && getMinutes() >= 0 && getSeconds() >= 0;
    }
    
    /**
     * Skip one second in the time stamp.
     */
    public void skipSecond() {
        setSeconds(getSeconds() + 1);
        if (getSeconds() == 60) {
            setSeconds(0);
            setMinutes(getMinutes() + 1);
            if (getMinutes() == 60) {
                setMinutes(0);
                setHours(getHours() + 1);
            }
        }
    }

    /**
     * Skip one minute in the time stamp.
     */
    public void skipMinute() {
        setMinutes(getMinutes() + 1);
        if (getMinutes() == 60) {
            setMinutes(0);
            setHours(getHours() + 1);
        }
    }

    /**
     * Skip one hour in the time stamp.
     */
    public void skipHour() {
        setHours(getHours() + 1);
    }

    /**
     * Skips time by TimeStamp time
     * @param time
     */
    public void SkipTime(TimeStamp time) {
        setHours(getHours() + time.getHours());
        setMinutes(getMinutes() + time.getMinutes());
        setSeconds(getSeconds() + time.getSeconds());

        //correcting time
        setMinutes(getMinutes() + getSeconds()/60);
        setSeconds(getSeconds() - (getSeconds()/60)*60);
        setHours(getHours() + getMinutes()/60);
        setMinutes(getMinutes() - (getMinutes()/60)*60);
    }

    /**
     * Clones time as a new TimeStamp
     */
    public TimeStamp clone() {
        return new TimeStamp(getHours(),getMinutes(),getSeconds());
    }

    /**
     * Returns TimeStamp as a string
     */
    public String TimeToString() {
        return getHours() + ":" + getMinutes() + ":" + getSeconds();
    }
}
