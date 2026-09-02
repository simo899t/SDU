package Exercise3;

import javax.imageio.ImageIO;

public class imageUtils {
    public static void main(String[] args) {
        Image image = Image.fromFile("/Users/simon/OneDrive/Pictures/clank.png");
        //image = flipHorizontal(image);
        //image = flipVertical(image);
        //image = rotateRight(image);
        //image = rotateLeft(image);
        //image = rotatehalf(image);
        //image = stretchHorizontal(image);
        //image = stretchVertical(image);
        //image = crop(image, 50, 50, 150, 150);
        //image = switchRedGreen(image);
        //image = switchRedBlue(image);
        //image = switchGreenBlue(image);
        //image = grayscaleAverage(image);
        //image = grayscaleLightness(image);
        //image = grayscaleLuminosity(image);
        //System.out.println(averageColor(image));
        //image = resample(image);
        image.display();
    }

    public static Image flipHorizontal(Image Image) {
        int width = Image.width();
        int height = Image.height();
        Image flippedImage = new Image(width, height);
        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                flippedImage.setPixel(i, height-j-1, Image.pixel(i, j));
            }
        }
        return flippedImage;
    }

    public static Image Enlarge(Image image, int n) {
        for (int i = 0; i < n; i++) {
            image = stretchHorizontal(image);
            image = stretchVertical(image);
        }
        return image;
    }

    public static Image flipVertical(Image Image) {
        int width = Image.width();
        int height = Image.height();
        Image flippedImage = new Image(width, height);
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                flippedImage.setPixel(width-w-1, h, Image.pixel(w, h));
            }
        }
        return flippedImage;
    }

    public static Image rotateRight(Image image) {
        int width = image.width();
        int height = image.height();
        Image rotatedRight = new Image(height, width);
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                rotatedRight.setPixel(height-h-1, w, image.pixel(w, h));
            }
        }
        return rotatedRight;

    }

    public static Image rotateLeft(Image image) {
        int width = image.width();
        int height = image.height();
        Image rotatedLeft = new Image(height, width);
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                rotatedLeft.setPixel(h, width-w-1, image.pixel(w, h));
            }
        }
        return rotatedLeft;

    }

    public static Image rotatehalf(Image image) {
        Image rotatedhalf = rotateRight(image);
        rotatedhalf = rotateRight(rotatedhalf);
        return rotatedhalf;
    }

    public static Image stretchHorizontal(Image image) {
        int width = image.width();
        int height = image.height();
        Image stretchHorizontal = new Image(2*width, height);
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                stretchHorizontal.setPixel(2*w, h, image.pixel(w, h));
                stretchHorizontal.setPixel(2*w+1, h, image.pixel(w, h));
            }
        }
        return stretchHorizontal;
    }

    public static Image stretchVertical(Image image) {
        int width = image.width();
        int height = image.height();
        Image stretchVertical = new Image(width, 2*height);
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                stretchVertical.setPixel(w, 2*h, image.pixel(w, h));
                stretchVertical.setPixel(w, 2*h+1, image.pixel(w, h));
            }
        }
        return stretchVertical;
    }

    public static Image crop(Image image, int x, int y, int width, int height) {
        Image crop = new Image((width-x), (height-y));
        for (int w = x; w < width; w++) {
            for (int h = y; h < height; h++) {
                crop.setPixel(w-x, h-y, image.pixel(w, h));
                crop.setPixel(w-x, h-y, image.pixel(w, h));
            }
        }
        return crop;
    }

    public static Image switchRedGreen(Image image) {
        int width = image.width();
        int height = image.height();
        Image switchRedGreen = new Image(width, height);
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                Color pixel = image.pixel(w, h);
                int redComponent = pixel.red();
                int greenComponent = pixel.green();
                int blueComponent = pixel.blue();
                pixel = new Color(greenComponent, redComponent, blueComponent);
                switchRedGreen.setPixel(w, h, pixel);
                }
            }
        return switchRedGreen;
    }

    public static Image switchRedBlue(Image image) {
        int width = image.width();
        int height = image.height();
        Image switchRedBlue = new Image(width, height);
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                Color pixel = image.pixel(w, h);
                int redComponent = pixel.red();
                int greenComponent = pixel.green();
                int blueComponent = pixel.blue();
                pixel = new Color(blueComponent, greenComponent, redComponent);
                switchRedBlue.setPixel(w, h, pixel);
                }
            }
        return switchRedBlue;
    }

    public static Image switchGreenBlue(Image image) {
        int width = image.width();
        int height = image.height();
        Image switchGreenBlue = new Image(width, height);
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                Color pixel = image.pixel(w, h);
                int redComponent = pixel.red();
                int greenComponent = pixel.green();
                int blueComponent = pixel.blue();
                pixel = new Color(redComponent, blueComponent, greenComponent);
                switchGreenBlue.setPixel(w, h, pixel);
                }
            }
        return switchGreenBlue;
    }

    public static Image grayscaleAverage(Image image) {
        int width = image.width();
        int height = image.height();
        Image grayscaleAverage = new Image(width, height);
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                Color pixel = image.pixel(w, h);
                int redComponent = pixel.red();
                int greenComponent = pixel.green();
                int blueComponent = pixel.blue();
                int average = (redComponent + greenComponent + blueComponent) / 3;
                pixel = new Color(average,average,average);
                grayscaleAverage.setPixel(w, h, pixel);
                }
            }
        return grayscaleAverage;
    }

    public static Image grayscaleLightness(Image image) {
        int width = image.width();
        int height = image.height();
        Image grayscaleLightness = new Image(width, height);
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                Color pixel = image.pixel(w, h);
                int redComponent = pixel.red();
                int greenComponent = pixel.green();
                int blueComponent = pixel.blue();
                int lightness = Math.min(redComponent, Math.min(greenComponent, blueComponent))
                     + Math.max(redComponent, Math.max(greenComponent, blueComponent)) / 2;
                pixel = new Color(lightness,lightness,lightness);
                grayscaleLightness.setPixel(w, h, pixel);
                }
            }
        return grayscaleLightness;
    }

    public static Image grayscaleLuminosity(Image image) {
        int width = image.width();
        int height = image.height();
        Image grayscaleLuminosity = new Image(width, height);
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                Color pixel = image.pixel(w, h);
                int redComponent = pixel.red();
                int greenComponent = pixel.green();
                int blueComponent = pixel.blue();
                int luminosity = (int) (0.3*redComponent + 0.59*greenComponent + 0.11*blueComponent);
                pixel = new Color(luminosity,luminosity,luminosity);
                grayscaleLuminosity.setPixel(w, h, pixel);
                }
            }
        return grayscaleLuminosity;
    }

    public static int averageColor(Image image) {
        int width = image.width();
        int height = image.height();
        int averageSum = 0;
        for (int w = 0; w < width; w++) {
            for (int h = 0; h < height; h++) {
                Color pixel = image.pixel(w, h);
                int redComponent = pixel.red();
                int greenComponent = pixel.green();
                int blueComponent = pixel.blue();
                averageSum += (int) (redComponent + greenComponent + blueComponent)/3;
                }
            }
        return averageSum/(width*height);
    }

    public static Image resample(Image image) {
        int width = image.width();
        int height = image.height();
        Image resampled = new Image(width, height);
        for (int w = 1; w < width-1; w++) {
            for (int h = 1; h < height-1; h++) {
                Color pixel = image.pixel(w, h);
                int redComponent = pixel.red();
                int greenComponent = pixel.green();
                int blueComponent = pixel.blue();
                for (int i = w - 1; i < w + 2; i++) {
                    for (int j = h - 1; j < h + 2; j++) {
                        if (j == h) {
                            continue;
                        }
                        pixel = image.pixel(i, j);
                        redComponent += pixel.red();
                        greenComponent += pixel.green();
                        blueComponent += pixel.blue();   
                    }
                }
                pixel = new Color(redComponent/9,greenComponent/9,blueComponent/9);
                resampled.setPixel(w, h, pixel);
            }
        }
        return resampled;
    }
}




