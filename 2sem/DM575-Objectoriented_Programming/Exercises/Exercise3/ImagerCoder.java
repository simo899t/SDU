package Exercise3;

import static Exercise3.imageUtils.stretchHorizontal;
import static Exercise3.imageUtils.stretchVertical;

public class ImagerCoder {

    public static void main(String[] args) {
        Image image = Image.fromFile("/C:/Users/simon/OneDrive/Pictures/clank.png");
        String key = "a";
        image.display();
        encrypt(image, key);
        image.display();
        decrypt(image, key);
        // enlargen the image
        //for (int i = 0; i < 2; i++) {
        //    image = stretchHorizontal(image);
        //    image = stretchVertical(image);
        //}
        image.display();
    }

    /**
     * Encrypts the image using the key.
     * Requires the key to be at least 3 characters long.
     * @param image
     * @param key
     */
    public static void encrypt(Image image, String key) {
        int width = image.width();
        int height = image.height();

        // get the first pixel
        Color prevPixel = image.pixel(0, 0);
        int redComponent = (prevPixel.red() + (int)  key.charAt(0))%256;
        int greenComponent = (prevPixel.green() + (int)  key.charAt(0))%256;
        int blueComponent = (prevPixel.blue() + (int)  key.charAt(0))%256;
        
        //redComponent = Math.floorMod(pixel.red() + key.charAt(0),256);
        //System.out.println(redComponent);

        // set the first pixel
        prevPixel = new Color(redComponent, greenComponent, blueComponent);
        image.setPixel(0, 0, prevPixel);


        // loop through the rest of the pixels
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                if (w == 0 && h == 0) {
                    continue;
                }
                // get the pixel
                Color pixel = image.pixel(w, h);
                redComponent = (prevPixel.red() + pixel.red() + (int)  key.charAt(0))%256;
                greenComponent = (prevPixel.blue() + pixel.green() + (int)  key.charAt(0))%256;
                blueComponent = (prevPixel.blue() + pixel.blue() + (int)  key.charAt(0))%256;

                

                //redComponent = Math.floorMod(pixel2.red() + prevRed + key.charAt(0),256);
                //System.out.println(redComponent);

                // set the pixel
                image.setPixel(w, h, new Color(redComponent, greenComponent, blueComponent));
                prevPixel = image.pixel(w, h);
            }
        }
    }

    /**
     * Returns the modulus of a and b.
     * @param a
     * @param b
     * @return
     */
    public static int myMod(int a, int b) {
        int result = a % b;
        if (result < 0) {
            result += b;
        }
        return result;
    }
    

    public static void decrypt(Image image, String key) {
        int width = image.width();
        int height = image.height();
    
        // get the first pixel
        Color firstPixel = image.pixel(0, 0);
        int redComponent = myMod(firstPixel.red() - (int) key.charAt(0), 256);
        int greenComponent = myMod(firstPixel.green() - (int) key.charAt(0), 256);
        int blueComponent = myMod(firstPixel.blue() - (int) key.charAt(0), 256);
    
        // set the first pixel
        Color pixel = new Color(redComponent, greenComponent, blueComponent);
        image.setPixel(0, 0, pixel);
    
        // loop through the rest of the pixels
        Color prevPixel = firstPixel;
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                if (w == 0 && h == 0) {
                    continue; // Skip the first pixel as it is already processed
                }
    
                // get the pixel
                pixel = image.pixel(w, h);
                redComponent = myMod(pixel.red() - prevPixel.red() - (int) key.charAt(0), 256);
                greenComponent = myMod(pixel.green() - prevPixel.green() - (int) key.charAt(0), 256);
                blueComponent = myMod(pixel.blue() - prevPixel.blue() - (int) key.charAt(0), 256);
                
                // update prevPixel to the current pixel for the next iteration
                prevPixel = pixel;

                // set the pixel
                pixel = new Color(redComponent, greenComponent, blueComponent);
                image.setPixel(w, h, pixel);
    
                
            }
        }
    }
}