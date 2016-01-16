import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

import javax.imageio.ImageIO;

public class Base {
    private static final File PROJECT_ROOT = new File(Base.class
            .getProtectionDomain().getCodeSource().getLocation().getFile())
                    .getParentFile();
    public static final File[] IMAGE_LOCS;
    public static final BufferedImage[] IMAGES;
    static {
        System.out.println(PROJECT_ROOT);
        IMAGE_LOCS = new File(PROJECT_ROOT, "raw").listFiles();
        Arrays.sort(IMAGE_LOCS);
        IMAGES = new BufferedImage[IMAGE_LOCS.length];
        for (int i = 0; i < IMAGE_LOCS.length; i++) {
            try {
                IMAGES[i] = ImageIO.read(IMAGE_LOCS[i]);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        System.out.println("Loading complete");
    }
    public static BufferedImage combine(BiFunction<Color, Color, Color> f,
            BufferedImage a, BufferedImage b) {
        BufferedImage combined = new BufferedImage(a.getWidth(), a.getHeight(),
                a.getType());
        for (Coordinate<Integer> xy : Mathematics.cartRange(0, a.getWidth(), 0,
                a.getHeight(), u -> true)) {
            combined.setRGB(xy.x, xy.y, f.apply(new Color(a.getRGB(xy.x, xy.y)),
                    new Color(b.getRGB(xy.x, xy.y))).getRGB());
        }
        return combined;
    }
    public static BiFunction<Color, Color, Color> layerMonochrome(Color a,
            Color b, Color c, Color d) {
        return (x, y) -> {
            boolean xblack = x.equals(Color.black);
            boolean yblack = y.equals(Color.black);
            if (xblack) return yblack ? a : b;
            return yblack ? c : d;
        };
    }
    public static BufferedImage average(BufferedImage... ins) {
        BufferedImage avg = new BufferedImage(ins[0].getWidth(),
                ins[0].getHeight(), ins[0].getType());
        for (int x = 0; x < ins[0].getWidth(); x++) {
            for (int y = 0; y < ins[0].getHeight(); y++) {
                int r = 0, g = 0, b = 0;
                for (BufferedImage in : ins) {
                    Color color = new Color(in.getRGB(x, y));
                    r += color.getRed();
                    g += color.getGreen();
                    b += color.getBlue();
                }
                r /= ins.length;
                g /= ins.length;
                b /= ins.length;
                avg.setRGB(x, y, new Color(r, g, b).getRGB());
            }
        }
        return avg;
    }
    public static BufferedImage overlay(BufferedImage base, BufferedImage layer,
            Predicate<Color> empty) {
        BufferedImage avg = new BufferedImage(base.getWidth(), base.getHeight(),
                base.getType());
        for (Coordinate<Integer> xy : Mathematics.cartRange(0, base.getWidth(),
                0, base.getHeight(), u -> true)) {
            int color = layer.getRGB(xy.x, xy.y);
            if (empty.test(new Color(color))) {
                color = base.getRGB(xy.x, xy.y);
            }
            avg.setRGB(xy.x, xy.y, color);
        }
        return avg;
    }
    public static void write(BufferedImage toWrite, String path) {
        try {
            ImageIO.write(toWrite, "png", new File(PROJECT_ROOT, path));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    public static BufferedImage[] applyToAll(
            Function<BufferedImage, BufferedImage> f) {
        BufferedImage[] result = new BufferedImage[IMAGES.length];
        for (int i = 0; i < result.length; i++) {
            result[i] = f.apply(IMAGES[i]);
        }
        return result;
    }
}
