package Exercise3;

import static Exercise3.imageUtils.resample;

public class imageUtilsSE {

    public static void main(String[] args) {
        Image image = Image.fromFile("/Users/simon/OneDrive/Pictures/clank.png");
        addRectangle(image, 10, 10, 20, 20, Color.RED);
        addCircle(image, 40, 40, 20, Color.BLUE);
        


        
        image.display();
    }

    /**
     * Adds a rectangle to the image.
     * @param image
     * @param x
     * @param y
     * @param width
     * @param height
     * @param color
     */
    public static void addRectangle(Image image, int x, int y, int width, int height, Color color) {
        for (int i = x; i < x + width; i++) {
            for (int j = y; j < y + height; j++) {
                image.setPixel(i, j, color);
            }
        }
    }

    /**
     * Adds a circle to the image.
     * @param image
     * @param x
     * @param y
     * @param radius
     * @param color
     */
    public static void addCircle(Image image, int x, int y, int radius, Color color) {
        for (int i = x - radius; i < x + radius; i ++) {
            for (int j = y - radius; j < y + radius; j++) {
                // check if the pixel is within the circle
                // using the distance formula sqrt((x-a)^2 + (y-b)^2) <= r
                // where (a,b) is the center of the circle
                // and r is the radius
                // Using <= r to include the inner pixels
                if (Math.sqrt(Math.pow(i - x, 2) + Math.pow(j - y, 2)) <= radius) {
                    image.setPixel(i, j, color);
                }
            }
        }
    }





}